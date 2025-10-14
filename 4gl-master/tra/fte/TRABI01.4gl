##############################################################################
#Owner             => E.F.P.
#Programa TRABI01  => GENERA SOLICITUD DE TRASPASO DE ICEFAS		
#Fecha creacion    => 20 DE AGOSTO DE 1997
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 31 DE ENERO DEL 2005
#Ultima Mod        => ENERO DEL 2006.             
#Objetivo de la Mod=> La Modif consistio en que la Generacion de Solicitudes
#                     Tomara en cuenta tambien aquellas con status 9(REENVIADA)
#                     Ya que estas no se pueden pasar a status 1(CONFIRMADA).
#                     Se Modifico la Parte  donde ponia en  duro el origen
#                     traspaso , ya lo va a tomar directamente de la Tabla 
#                     safre_af:tra_mae_icefa_issste del campo origen_traspaso.
#Sistema           => TRA-ICE-ISSSTE
##############################################################################
DATABASE safre_af

GLOBALS

DEFINE v_tipo_sol LIKE afi_mae_afiliado.tipo_solicitud
DEFINE vmarca_causa SMALLINT
DEFINE ejecuta CHAR(300)
DEFINE xcodigo_marca SMALLINT,
       xcodigo_rechazo SMALLINT
DEFINE vnss            CHAR(11),
       vmarca_entra    SMALLINT,
       vmarca_estado   SMALLINT,
       vcodigo_rechazo SMALLINT,
       vusuario        CHAR(08)

    DEFINE reg_cza_sol_trasp RECORD #glo #reg_cza_sol_trasp
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        cve_ent_origen        CHAR(03) ,
        tipo_ent_destino      CHAR(02) ,
        ent_fed_envio_lote    CHAR(03) ,
        fech_presentacion     CHAR(08) ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(02)
    END RECORD

    DEFINE reg_det_sol_trasp RECORD #glo #reg_det_sol_trasp
        tipo_registro         CHAR(002)     ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        SMALLINT      ,
        orig_tipo_trasp       CHAR(002)     ,
        fech_presentacion     CHAR(008)     ,
        n_unico               CHAR(018)     ,
        n_seguro              CHAR(011)     ,
        rfc                   CHAR(013)     ,
        paterno               CHAR(040)     ,
        materno               CHAR(040)     ,
        nombres               CHAR(040)     ,
        cve_sector            CHAR(001)     ,
        fech_recep_solic      CHAR(008)     ,
        ident_lote_solic      CHAR(016)     ,
        n_seguro_ent          CHAR(011)     ,
        rfc_ent               CHAR(013)     ,
        num_ctrl_int_icefa    CHAR(030)     ,
        nombre_trab_icefa     CHAR(120)
    END RECORD

    DEFINE reg_sum_sol_trasp RECORD #glo #reg_sum_sol_trasp
        tipo_registro         CHAR(02) ,
        cantidad_reg_det      INTEGER
    END RECORD

    DEFINE reg_1 RECORD #glo #reg_1
        capturada             SMALLINT ,
        enviada               SMALLINT ,
        reenviada             SMALLINT ,
 	rech_conv             SMALLINT ,
	pendiente             SMALLINT ,
        pendiente_docto       SMALLINT 
    END RECORD

    DEFINE #glo #date
        f_fech_recep_solic    ,
        f_fentcons            ,
        f_fecha_solic_tra     ,
        HOY                   DATE
     
    DEFINE #glo #char
        x_n_seguro            CHAR(011) ,
        RUTA                  CHAR(100) ,
        G_LISTA_1             CHAR(100) ,
        G_LISTA_2             CHAR(100) ,
        G_LISTA_3             CHAR(100) ,
        usuario               CHAR(008) ,
        c10_fecha_solic_tra   CHAR(010) ,
        c10_fentcons          CHAR(010) ,
        c40_paterno           CHAR(040) ,
        c40_materno           CHAR(040) ,
        c40_nombres           CHAR(040) ,
        enter    	      CHAR(001) ,
        cat                   CHAR(300) ,
        c8_HOY                CHAR(008) ,
        HORA                  CHAR(005)

    DEFINE #glo #smallint
        sw_1                  ,
        s_lotes_num           ,
        s_lotes_correlativo   ,
        s_codigo_afore        SMALLINT

    DEFINE #glo #integer
        i_correlativo         INTEGER

    DEFINE #glo #decimal
        cont_reg              DECIMAL(10,0)

    DEFINE folio_val LIKE tra_det_aut_issste.folio_interno
    DEFINE i     INTEGER    
    DEFINE reg RECORD LIKE tra_mae_icefa_issste.* 
    DEFINE env_sol RECORD
           tipo_criterio  LIKE tra_det_aut_issste.tipo_criterio,
           folio_interno  LIKE tra_det_aut_issste.folio_interno,
           cve_ced_cuenta LIKE tra_det_aut_issste.cve_ced_cuenta,
           estado         LIKE tra_det_aut_issste.estado
                  END RECORD 
    DEFINE est_acept      CHAR(020) 
    DEFINE g_param_taa    RECORD LIKE seg_modulo.* 
    DEFINE rep RECORD 
           cve_ced_cuenta LIKE tra_det_aut_issste.cve_ced_cuenta,
           tipo_criterio  LIKE tra_det_aut_issste.tipo_criterio,
           total          INTEGER
               END RECORD
    DEFINE raz_social   LIKE tab_afore_local.razon_social
    DEFINE cod_afore    LIKE tab_afore_local.codigo_afore
    DEFINE g_listita  CHAR(100) 
    DEFINE permisos   CHAR(100) 
    DEFINE proc_edo_cod CHAR(20)
END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST  ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    WHENEVER ERROR CONTINUE 
        DATABASE safre_tmp
        DROP TABLE res_env_sol

        CREATE  TABLE res_env_sol( tipo_criterio  SMALLINT,
                                   folio_interno  INTEGER,
                                   cve_ced_cuenta CHAR(03),
                                   estado         SMALLINT)

       CREATE INDEX rrr  ON res_env_sol(folio_interno,tipo_criterio)
       CREATE INDEX rrr1 ON res_env_sol(folio_interno)
				  
    WHENEVER ERROR STOP

    DATABASE safre_af

    CALL init()

    OPEN WINDOW trab0011 AT 4,4 WITH FORM "TRAB0011" ATTRIBUTE(BORDER)
    DISPLAY" TRABI01  GENERA SOLICITUD DE TRASPASO ICEFA-AFORE ISSSTE                      " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY "                          < CTRL-C> Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)

    WHILE TRUE
        PROMPT "ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                DISPLAY"PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE) SLEEP 2
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CALL genera_cza_sol_trasp()   #gcst
    CALL genera_det_sol_trasp()   #gdst
    CALL genera_sum_sol_trasp()   #gsst
    CALL envio_sar92() 

    LET cat =    RUTA CLIPPED,"/SOLISSSTE",
                 HOY USING"YYYYMMDD","-", HORA[1,2],HORA[4,5]
    LET cat = cat CLIPPED

    DISPLAY "ARCHIVO GENERADO EN: ",cat  AT 17,1 ATTRIBUTE(REVERSE)

    PROMPT "PROCESO FINALIZADO...PRESIONE <ENTER> PARA GENERAR ARCHIVO"
    FOR CHAR enter

    LET cat = "cat ",G_LISTA_1 CLIPPED," ",G_LISTA_2 CLIPPED," ",
                     G_LISTA_3 CLIPPED," > ",RUTA CLIPPED,"/SOLISSSTE",
                     HOY USING"YYYYMMDD","-", HORA[1,2],HORA[4,5]

    RUN cat

    UPDATE tab_lote
    SET    lotes_num   = s_lotes_num
    WHERE  lotes_fecha = HOY
    AND    lotes_cod   = 6

    CLOSE WINDOW trab0011

END MAIN

FUNCTION init()
#--------------

    LET HOY     = TODAY
    LET HORA    = TIME
    LET c8_HOY  = HOY USING"YYYYMMDD"

    SELECT A.ruta_envio
    INTO   RUTA
    FROM   seg_modulo A
    WHERE  A.modulo_cod = "tra"

    SELECT lotes_num          ,
           lotes_correlativo 
    INTO   s_lotes_num ,
           s_lotes_correlativo
    FROM   tab_lote
    WHERE  lotes_fecha = HOY
    AND    lotes_cod   = 6

    IF STATUS = NOTFOUND THEN
        LET s_lotes_num = 1
        INSERT INTO tab_lote VALUES(HOY,6,"TRASPASOS",0,1)
    ELSE
        LET s_lotes_num = s_lotes_num + 1
    END IF

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    SELECT  estado
    INTO    reg_1.capturada
    FROM    tra_status
    WHERE   des_estado = "CONFIRMADA"

    SELECT  estado
    INTO    reg_1.rech_conv
    FROM    tra_status
    WHERE   des_estado = "RECH_CONV"

    SELECT  estado
    INTO    reg_1.reenviada
    FROM    tra_status
    WHERE   des_estado = "REENVIADA"

    SELECT  estado
    INTO    reg_1.enviada
    FROM    tra_status
    WHERE   des_estado = "ENVIADA"

    SELECT  estado
    INTO    reg_1.pendiente
    FROM    tra_status
    WHERE   des_estado = "PENDIENTE ASIG"

    SELECT  estado
    INTO    reg_1.pendiente_docto
    FROM    tra_status
    WHERE   des_estado = "PENDIENTE POR DOCTOS"

    SELECT *
       INTO g_param_taa.*
       FROM seg_modulo
    WHERE  modulo_cod = 'tra' 

    SELECT estado_cod
    INTO   proc_edo_cod 
    FROM   tra_aut_estado
    WHERE  estado_descripcion = "ENVIADA AL MAESTRO DE ICEFAS " 

    LET proc_edo_cod = 30

END FUNCTION

FUNCTION genera_cza_sol_trasp()
#gcst--------------------------
    LET reg_cza_sol_trasp.tipo_registro      = "01"
    LET reg_cza_sol_trasp.ident_servicio     = "02"
    LET reg_cza_sol_trasp.ident_operacion    = "01"
    LET reg_cza_sol_trasp.tipo_ent_origen    = "01"
    LET reg_cza_sol_trasp.cve_ent_origen     = s_codigo_afore 
    LET reg_cza_sol_trasp.tipo_ent_destino   = "07"
    LET reg_cza_sol_trasp.ent_fed_envio_lote = "009"
    LET reg_cza_sol_trasp.fech_presentacion  = c8_HOY
    LET reg_cza_sol_trasp.consec_lote_dia    = s_lotes_num
    LET reg_cza_sol_trasp.cve_mod_recepcion  = "02"

    LET G_LISTA_1 = RUTA CLIPPED,"/CST"
    START REPORT listado_1 TO G_LISTA_1
        OUTPUT TO REPORT listado_1(reg_cza_sol_trasp.*) #1
    FINISH REPORT listado_1
END FUNCTION

FUNCTION genera_det_sol_trasp()
#gdst--------------------------

DEFINE ee char(050)

LET ee = "SET PDQPRIORITY 100"
LET ee = ee CLIPPED

PREPARE pd FROM ee
EXECUTE pd 


    DECLARE cur_1 CURSOR FOR
    SELECT  A.n_seguro        ,
            A.icefa_cod       ,#cve_ced_cuenta
            A.nss             ,#n_seguro_ent
            A.rfc             ,#rfc_ent
            A.nro_int_cta     ,#num_ctrl_int_icefa
            A.paterno         ,
            A.materno         ,
            A.nombres         , #nombre_trab_icefa
            A.fecha_solic_tra , #se sustituye por fech_solic_tra allianz
            A.correlativo     ,
            A.origen_traspaso   #origen_traspaso
    FROM    safre_af:tra_mae_icefa_issste A 
    WHERE   A.status         in (reg_1.capturada,      #CONFIRMADA     (1)
                                 reg_1.rech_conv,      #RECH_CONV      (21)
				 reg_1.pendiente,      #PENDIENTE ASIG (30)
                                 reg_1.pendiente_docto,#NO EXI EN EL CAT 
 	                         reg_1.reenviada )     #REENVIADA       (9)
    ORDER BY A.n_seguro

    LET reg_det_sol_trasp.tipo_registro     = "02"
    LET reg_det_sol_trasp.tipo_recep_cuenta = "01"
    LET reg_det_sol_trasp.cve_recep_cuenta  = s_codigo_afore
    LET reg_det_sol_trasp.tipo_ced_cuenta   = "07"
    LET reg_det_sol_trasp.fech_presentacion = c8_HOY
    LET reg_det_sol_trasp.cve_sector        = "2"

    LET reg_det_sol_trasp.ident_lote_solic  = "01",
                                              s_codigo_afore USING"&&&",
                                              c8_HOY,
                                              s_lotes_num   USING"&&&"

    LET cont_reg  = 0

    LET G_LISTA_2 = RUTA CLIPPED,"/DST"

    START REPORT listado_2 TO G_LISTA_2

    LET sw_1 = 0

        FOREACH cur_1 INTO x_n_seguro                          ,
                           reg_det_sol_trasp.cve_ced_cuenta    ,
                           reg_det_sol_trasp.n_seguro_ent       ,
                           reg_det_sol_trasp.rfc_ent            ,
                           reg_det_sol_trasp.num_ctrl_int_icefa ,
                           c40_paterno                          ,
                           c40_materno                          ,
                           c40_nombres                          ,
                           f_fecha_solic_tra                    ,
                           i_correlativo                        ,
                           reg_det_sol_trasp.orig_tipo_trasp  

SELECT A.tipo_solicitud
INTO   v_tipo_sol 
FROM   afi_mae_afiliado A
WHERE  A.n_seguro = x_n_seguro

IF v_tipo_sol = 5 THEN

    UPDATE tra_mae_icefa_issste
    SET    tra_mae_icefa_issste.status = pendiente
    WHERE  correlativo = i_correlativo

    CONTINUE FOREACH

END IF

SELECT "OK"
FROM  cta_act_marca A
WHERE nss = x_n_seguro 
AND   A.marca_cod = 5
AND   A.marca_causa  between 200 and 299
GROUP BY 1

IF STATUS <> NOTFOUND THEN

 UPDATE tra_mae_icefa_issste 
 SET    tra_mae_icefa_issste.status = reg_1.rech_conv
 WHERE  correlativo = i_correlativo

 CONTINUE FOREACH

END IF

LET vnss            = x_n_seguro
LET vmarca_entra    = 260
LET vmarca_estado   = 0
LET vcodigo_rechazo = 0

SELECT user
INTO   vusuario 
FROM   tab_afore_local


LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(","'",vnss    ,"'"   ,",",
					    vmarca_entra   ,",",
				            i_correlativo  ,",",
				            vmarca_estado  ,",",
     		                            vcodigo_rechazo,",",
		                            vmarca_causa,",",
			                    "'',",
			                     "'",vusuario,"')"


LET ejecuta = ejecuta CLIPPED
PREPARE clausula_spl FROM ejecuta
DECLARE cursor_marca CURSOR FOR clausula_spl
OPEN cursor_marca
FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo
CLOSE cursor_marca

IF xcodigo_rechazo <> 0 THEN

 UPDATE tra_mae_icefa_issste 
 SET    tra_mae_icefa_issste.status = reg_1.rech_conv
 WHERE  correlativo = i_correlativo

 CONTINUE FOREACH

END IF

             LET cont_reg = cont_reg + 1  
             DISPLAY "NRO. DE REGISTROS PROCESADOS ",cont_reg at 10,8

             SELECT  B.n_unico  ,
                     B.n_seguro ,
                     B.n_rfc    ,
                     B.paterno  ,
                     B.materno  ,
                     B.nombres  ,
                     B.fentcons
             INTO
                     reg_det_sol_trasp.n_unico  ,
                     reg_det_sol_trasp.n_seguro ,
                     reg_det_sol_trasp.rfc      ,
                     reg_det_sol_trasp.paterno  ,
                     reg_det_sol_trasp.materno  ,
                     reg_det_sol_trasp.nombres  ,
                     f_fentcons
             FROM    afi_mae_afiliado B
             WHERE   B.n_seguro = x_n_seguro

             LET reg_det_sol_trasp.nombre_trab_icefa[01,40]  = c40_paterno 
             LET reg_det_sol_trasp.nombre_trab_icefa[41,80]  = c40_materno
             LET reg_det_sol_trasp.nombre_trab_icefa[81,120] = c40_nombres

             IF f_fecha_solic_tra IS NULL THEN
                 LET c10_fentcons = f_fentcons
                 LET reg_det_sol_trasp.fech_recep_solic =c10_fentcons[07,10],
                                                         c10_fentcons[01,02],
                                                         c10_fentcons[04,05]
                 LET f_fech_recep_solic = f_fentcons
             ELSE
             LET c10_fecha_solic_tra = f_fecha_solic_tra
             LET reg_det_sol_trasp.fech_recep_solic =c10_fecha_solic_tra[07,10],
                                                     c10_fecha_solic_tra[01,02],
                                                     c10_fecha_solic_tra[04,05]
             LET f_fech_recep_solic = f_fecha_solic_tra
             END IF

             LET sw_1 = 1

             OUTPUT TO REPORT listado_2(reg_det_sol_trasp.*) #2

INSERT INTO est_0961 VALUES (reg_det_sol_trasp.cve_ced_cuenta,
                             reg_det_sol_trasp.fech_presentacion,
                             reg_det_sol_trasp.fech_presentacion)

        UPDATE tra_mae_icefa_issste
        SET    tra_mae_icefa_issste.status          = reg_1.enviada      ,
               tra_mae_icefa_issste.lote_genera     = s_lotes_num        ,
               tra_mae_icefa_issste.fecha_genera    = TODAY              ,
               tra_mae_icefa_issste.fecha_proceso   = TODAY              ,
               tra_mae_icefa_issste.fecha_solic_tra = f_fech_recep_solic ,
               tra_mae_icefa_issste.n_envios = tra_mae_icefa_issste.n_envios + 1
        WHERE  tra_mae_icefa_issste.correlativo = i_correlativo

        UPDATE tra_det_atm_issste
        SET    tra_det_atm_issste.estado      = reg_1.enviada ,
               tra_det_atm_issste.fecha_edo   = TODAY     
        WHERE  tra_det_atm_issste.n_seguro    = x_n_seguro         
        AND  tra_det_atm_issste.n_seguro_ent  = reg_det_sol_trasp.n_seguro_ent
        AND  tra_det_atm_issste.rfc_ent       = reg_det_sol_trasp.rfc_ent 
        AND  tra_det_atm_issste.cve_ced_cuenta = reg_det_sol_trasp.cve_ced_cuenta
        AND  tra_det_atm_issste.nro_ctrl_icefa= reg_det_sol_trasp.num_ctrl_int_icefa
        AND  tra_det_atm_issste.estado       = 1 # CONFIRMADA     

        UPDATE tra_det_aut_issste
        SET    tra_det_aut_issste.estado      = reg_1.enviada ,
               tra_det_aut_issste.fecha_edo   = TODAY     
        WHERE  tra_det_aut_issste.n_seguro    = x_n_seguro         
        AND  tra_det_aut_issste.n_seguro_ent  = reg_det_sol_trasp.n_seguro_ent
        AND  tra_det_aut_issste.rfc_ent       = reg_det_sol_trasp.rfc_ent 
        AND  tra_det_aut_issste.cve_ced_cuenta = reg_det_sol_trasp.cve_ced_cuenta
        AND  tra_det_aut_issste.nro_ctrl_icefa= reg_det_sol_trasp.num_ctrl_int_icefa
        AND  tra_det_aut_issste.estado       = 1 # CONFIRMADA     
      
             IF cont_reg = 10000 THEN
                 EXIT FOREACH
             END IF
        END FOREACH

        IF sw_1 = 0 THEN
            DISPLAY "NO SE ENCONTRARON REGISTROS " AT 19,1 ATTRIBUTE(REVERSE)
            SLEEP 3
            EXIT PROGRAM
        END IF
    FINISH REPORT listado_2
END FUNCTION

FUNCTION genera_sum_sol_trasp()
#gsst--------------------------
    LET reg_sum_sol_trasp.tipo_registro     = "09"
    LET reg_sum_sol_trasp.cantidad_reg_det  = cont_reg

    LET G_LISTA_3 = RUTA CLIPPED,"/SST"

    START REPORT listado_3 TO G_LISTA_3
        OUTPUT TO REPORT listado_3(reg_sum_sol_trasp.*) #3
    FINISH REPORT listado_3
END FUNCTION

REPORT listado_1(reg_cza_sol_trasp)
#1---------------------------------
    DEFINE reg_cza_sol_trasp RECORD
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        cve_ent_origen        CHAR(03) ,
        tipo_ent_destino      CHAR(02) ,
        ent_fed_envio_lote    CHAR(03) ,
        fech_presentacion     CHAR(08) ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(02)
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
            COLUMN 001,reg_cza_sol_trasp.tipo_registro      ,
            COLUMN 003,reg_cza_sol_trasp.ident_servicio     ,
            COLUMN 005,reg_cza_sol_trasp.ident_operacion    ,
            COLUMN 007,reg_cza_sol_trasp.tipo_ent_origen    ,
            COLUMN 009,reg_cza_sol_trasp.cve_ent_origen     ,
            COLUMN 012,reg_cza_sol_trasp.tipo_ent_destino   ,
            COLUMN 017,reg_cza_sol_trasp.ent_fed_envio_lote ,
            COLUMN 020,reg_cza_sol_trasp.fech_presentacion  ,
            COLUMN 028,reg_cza_sol_trasp.consec_lote_dia    USING"&&&",
            COLUMN 031,reg_cza_sol_trasp.cve_mod_recepcion  ,
            COLUMN 033,698 SPACES
END REPORT

REPORT listado_2(reg_det_sol_trasp)
#2---------------- -----------------
    DEFINE reg_det_sol_trasp RECORD #loc #reg_det_sol_trasp
        tipo_registro         CHAR(002)     ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        SMALLINT      ,
        orig_tipo_trasp       CHAR(002)     ,
        fech_presentacion     CHAR(008)     ,
        n_unico               CHAR(018)     ,
        n_seguro              CHAR(011)     ,
        rfc                   CHAR(013)     ,
        paterno               CHAR(040)     ,
        materno               CHAR(040)     ,
        nombres               CHAR(040)     ,
        cve_sector            CHAR(001)     ,
        fech_recep_solic      CHAR(008)     ,
        ident_lote_solic      CHAR(016)     ,
        n_seguro_ent          CHAR(011)     ,
        rfc_ent               CHAR(013)     ,
        num_ctrl_int_icefa    CHAR(030)     ,
        nombre_trab_icefa     CHAR(120)
    END RECORD

    DEFINE tipo_sol           SMALLINT

    OUTPUT
        PAGE LENGTH 1
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW 

        SELECT A.tipo_solicitud
        INTO tipo_sol
           FROM afi_mae_afiliado A
        WHERE A.n_seguro = reg_det_sol_trasp.n_seguro
        IF ( tipo_sol = 8 ) THEN
           LET reg_det_sol_trasp.n_seguro = "           "
        END IF

        PRINT
            COLUMN 001,reg_det_sol_trasp.tipo_registro      ,
            COLUMN 003,cont_reg          USING"&&&&&&&&&&"  ,#cont_servicio
            COLUMN 013,reg_det_sol_trasp.tipo_recep_cuenta  ,
            COLUMN 015,reg_det_sol_trasp.cve_recep_cuenta   ,
            COLUMN 018,reg_det_sol_trasp.tipo_ced_cuenta    ,
            COLUMN 020,reg_det_sol_trasp.cve_ced_cuenta USING"&&&" ,
            COLUMN 023,reg_det_sol_trasp.orig_tipo_trasp USING "&&" ,
            COLUMN 025,reg_det_sol_trasp.fech_presentacion  ,
            COLUMN 041,reg_det_sol_trasp.n_unico            ,
            COLUMN 059,reg_det_sol_trasp.n_seguro           ,
            COLUMN 085,reg_det_sol_trasp.rfc                ,
            COLUMN 098,reg_det_sol_trasp.paterno            ,
            COLUMN 138,reg_det_sol_trasp.materno            ,
            COLUMN 178,reg_det_sol_trasp.nombres            ,
            COLUMN 221,reg_det_sol_trasp.cve_sector         ,
            COLUMN 232,reg_det_sol_trasp.fech_recep_solic   ,
            COLUMN 240,reg_det_sol_trasp.ident_lote_solic   ,
            COLUMN 271,reg_det_sol_trasp.n_seguro_ent       ,
            COLUMN 282,reg_det_sol_trasp.rfc_ent            ,
            COLUMN 295,reg_det_sol_trasp.num_ctrl_int_icefa ,
            COLUMN 325,reg_det_sol_trasp.nombre_trab_icefa  ,
            COLUMN 445,286 SPACES
END REPORT

REPORT listado_3(reg_sum_sol_trasp)
#3---------------------------------
    DEFINE reg_sum_sol_trasp RECORD #loc #reg_sum_sol_trasp
        tipo_registro        CHAR(02) ,
        cantidad_reg_det     INTEGER
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
            COLUMN 001,reg_sum_sol_trasp.tipo_registro    ,
            COLUMN 003,reg_sum_sol_trasp.cantidad_reg_det USING"&&&&&&&&&" ,
            COLUMN 011,719 SPACES
END REPORT

FUNCTION envio_sar92()
#es92-----------------

   DECLARE e CURSOR FOR
      SELECT a.*
      FROM   safre_af:tra_mae_icefa_issste a
      WHERE  a.status       = 2 # enviada
      AND    a.fuente       = 2 # cruce ISSSTE
      AND    a.cve_sector   = 2 # sector publico
      AND    a.fecha_genera = HOY 

   FOREACH e INTO reg.*

      DECLARE e1 CURSOR FOR

         SELECT a.tipo_criterio ,
		          a.folio_interno ,
		          a.cve_ced_cuenta,
		          a.estado
         FROM   tra_det_aut_issste  a
         WHERE  a.n_seguro        = reg.n_seguro
         AND    a.n_seguro_ent    = reg.nss
         AND    a.rfc_ent         = reg.rfc
         AND    a.cve_ced_cuenta  = reg.icefa_cod
         AND    a.nro_ctrl_icefa  = reg.nro_int_cta
         AND    a.estado          = reg_1.enviada
     
      FOREACH e1 INTO env_sol.* 

         IF( env_sol.estado = proc_edo_cod ) THEN 
                
            INSERT INTO safre_tmp:res_env_sol VALUES ( env_sol.tipo_criterio,
                                             env_sol.folio_interno,
                                             env_sol.cve_ced_cuenta,
                                             env_sol.estado   )
         ELSE
 
         END IF 

      END FOREACH

   END FOREACH 

      LET g_listita =  g_param_taa.ruta_listados CLIPPED,"/",usuario CLIPPED,".","ENVIOSAR92","_",HOY USING "DD-MM-YYYY"

   START REPORT rep_env_sol TO g_listita 
  
   LET i = 0 

   DATABASE safre_tmp
      UPDATE STATISTICS FOR TABLE res_env_sol
   DATABASE safre_af

   DECLARE e2 CURSOR FOR

      SELECT a.folio_interno   
      FROM   safre_tmp:res_env_sol a
      GROUP BY 1
      ORDER BY 1

   FOREACH e2 INTO folio_val

     FOR i = 1 TO 6     
        INITIALIZE rep.* TO NULL 

        SELECT a.cve_ced_cuenta  ,
               a.tipo_criterio   ,
               COUNT(*)
        INTO   rep.*
        FROM safre_tmp:res_env_sol a
        WHERE  a.folio_interno = folio_val 
          AND  a.tipo_criterio = i
        GROUP BY 1,2
        ORDER BY 1,2 

        IF (rep.total IS NULL OR 
            rep.total = 0 ) THEN 
           LET rep.total         = 0
           LET rep.tipo_criterio = i
        END IF 
       
      OUTPUT TO REPORT rep_env_sol(rep.*,folio_val) 

     END FOR 

   END FOREACH 

   FINISH REPORT rep_env_sol 

END FUNCTION

REPORT rep_env_sol(rep,folio_val)
#res-----------------------------

DEFINE folio_val LIKE tra_det_aut_issste.folio_interno
DEFINE rep RECORD 
       cve_ced_cuenta LIKE tra_det_aut_issste.cve_ced_cuenta,
       tipo_criterio  LIKE tra_det_aut_issste.tipo_criterio,
       total          INTEGER
           END RECORD
DEFINE icefa LIKE tra_det_aut_issste.cve_ced_cuenta

OUTPUT
   TOP MARGIN 1
   BOTTOM MARGIN 0
   LEFT MARGIN 0
   RIGHT MARGIN 0
   PAGE LENGTH 60

   FORMAT
        PAGE HEADER
        SELECT codigo_afore,razon_social
        INTO cod_afore,raz_social
        FROM tab_afore_local

        PRINT COLUMN 70,"Pagina:",PAGENO USING "<<<<"
        PRINT COLUMN 01, "_______________________________________________________________________________"
        PRINT COLUMN 01,"TRABI01",
              COLUMN 23,"REPORTE GENERAL DE ENVIO DE SOLICITUDES",
              COLUMN 68,TODAY USING "dd-mm-yyyy"
        PRINT COLUMN 24,"   TRASPASOS ICEFA-AFORE ISSSTE                "
        PRINT COLUMN 01,cod_afore USING "&&&","  ",raz_social CLIPPED
        PRINT COLUMN 01, "_______________________________________________________________________________"

        PRINT 

        BEFORE GROUP OF folio_val

           SELECT UNIQUE (cve_ced_cuenta)
              INTO icefa   
              FROM safre_tmp:res_env_sol
           WHERE folio_interno = folio_val

           PRINT COLUMN 01, "_______________________________________________________________________________"

--- LETRA PEQUEÑA PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'      
--- NEGRITA Y GRANDE PRINT '\033e\033(s218T\033(s9H\033(s7B'
--- PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'      

           PRINT COLUMN 01,"FOLIO: ",folio_val USING "#####",
                 COLUMN 08,"    ","ICEFA: ",icefa CLIPPED 
           PRINT COLUMN 01, "_______________________________________________________________________________"

           PRINT COLUMN 25,"CRITERIO",
                 COLUMN 50,"TOTAL"
           PRINT 

       ON EVERY ROW

          PRINT COLUMN 22,rep.tipo_criterio,
                COLUMN 42,rep.total
          PRINT

       AFTER GROUP OF folio_val

           PRINT COLUMN 01, "_______________________________________________________________________________"

           PRINT COLUMN 22,"TOTAL FOLIO: ",folio_val USING "#####",
                 COLUMN 46,GROUP SUM(rep.total) USING "###,###"
       PRINT  
       PRINT 

    ON LAST ROW
        SKIP 4 LINES
        PRINT COLUMN 2, "TOTAL DE REGISTROS ENVIADOS: ",
        SUM(rep.total) USING "<<<<"

    PAGE TRAILER
        SKIP 2 LINE
        PAUSE "Presione enter para continuar...."

END REPORT                      
