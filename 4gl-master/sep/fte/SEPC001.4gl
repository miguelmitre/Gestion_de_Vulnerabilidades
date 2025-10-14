################################################################################
#Owner             => E.F.P                                                    #
#Programa SEPM100  => RECIBE ARCHIVOS DE POSIBLES CUENTAS A SEPARAR            #
#Fecha creacion    => 8 DE AGOSTO DEL 2000                                     #
#Por               => ARMANDO RODRIGUEZ CASTROPAREDES                          #
#Sistema           => SEP 
#Modificacion      : Jesus Yañez Moreno
#Descripcion       : Se elimina el folio como control del proceso y se incluye 
#                    cifras de registros no encontrados. Se mantiene el folio 
#                    solo como referencia interna, se incluye tambien la
#                    generacion automatica de reporte 
#Fecha             : 3 abr 2010
################################################################################
DATABASE safre_af
GLOBALS

    DEFINE reg_cza_separa RECORD 
         tipo_registro       CHAR(2),
         ident_servicio      CHAR(2),
         ident_operacion     CHAR(2),
         tipo_ent_origen     CHAR(2),
         cve_ent_origen      CHAR(3),
         tipo_ent_destino    CHAR(2),
         cve_ent_destino     CHAR(3),
         fecha_lote          DATE,
         consec_lote         SMALLINT,
         resulta_operacion   CHAR(2),
         diag_1              CHAR(3),
         diag_2              CHAR(3),
         diag_3              CHAR(3),
         estado              smallint
    END RECORD 

    DEFINE reg_det_separa RECORD 
        tipo_registro         CHAR(2),
        cont_servicio         INTEGER,
        nss                   CHAR(11),
        rfc                   CHAR(13),
        curp                  CHAR(18),
        tipo_ent_admon        CHAR(2),
        clave_admon           CHAR(3),
        paterno               CHAR(40),
        materno               CHAR(40),
        nombre                CHAR(40),
        fecha_nacimiento      DATE,
        ent_nacimiento        CHAR(2),
        sexo                  CHAR(1),
        nombre_procanase      CHAR(50),
        fecha_afiliacion      DATE,
        fecha_marca_infosar   DATE,
        diag_confronta        CHAR(2),
        clasifica_separacion  char(1),
        credito_infonavit     char(1),
        resulta_operacion     CHAR(2),
        diag_proceso1         CHAR(3),
        diag_proceso2         CHAR(3),
        diag_proceso3         CHAR(3),
        traspaso_previo       CHAR(02)
    END RECORD 

    DEFINE reg_sum_separa RECORD 
        tipo_registro         CHAR(02),
        total_detalle_02      INTEGER ,
        total_detalle_03      INTEGER 
    END RECORD 

    DEFINE #char
        carga_reg             CHAR(330),
        usuario               CHAR(008),
        enter    	      CHAR(001),
        motivo_rechazo_1      CHAR(003),
        motivo_rechazo_2      CHAR(003),
        motivo_rechazo_3      CHAR(003),
        resulta_oper          CHAR(002),
        generar               CHAR(020),
        nombre_archivo        CHAR(020),
        archivo_separa        CHAR(200),
        c10_fecha_presenta    CHAR(010),
        c10_fecha_nac_sep     CHAR(010),
        c10_fecha_afilia      CHAR(010),
        c10_fecha_marca       CHAR(010) 

    DEFINE #smallint
        s_recibido,
        cuantos   ,
        cont      ,
        no_encontrados SMALLINT

    DEFINE #integer
        ultimo_folio          INTEGER
END GLOBALS

GLOBALS "SEPRPTS.4gl" --globales para reporte

MAIN
    OPTIONS
        PROMPT LINE LAST
        DEFER INTERRUPT

    LET cuantos = 0
    LET cont    = 0 
    LET no_encontrados = 0

    WHENEVER ERROR CONTINUE
        DROP TABLE sep_plano

        CREATE TEMP TABLE sep_plano
        (
         n_registros          CHAR(300)
        )
    WHENEVER ERROR STOP

 CALL init()

 OPEN WINDOW sepc0011 AT 2,2 WITH FORM "SEPC0011" ATTRIBUTE(BORDER)

 DISPLAY " [Esc] Iniciar [Ctrl-C] Salir                                                  " AT 1,1 ATTRIBUTE(REVERSE)  

 DISPLAY " SEPM100              RECIBE 1er ENVIO DE OP. 27                               " AT 3,1 ATTRIBUTE(REVERSE)

 DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

	INPUT BY NAME generar WITHOUT DEFAULTS
            BEFORE FIELD generar
               LET generar = NULL
               CLEAR FORM

	    AFTER FIELD generar
	       IF generar IS NULL THEN
	          ERROR "Campo NO puede ser NULO"
	          NEXT FIELD generar
	       END IF

               SELECT nombre
               INTO   nombre_archivo
               FROM   sep_ctr_archivo
               WHERE  nombre = generar
                
               IF STATUS <> NOTFOUND THEN
                  ERROR " Este archivo ya se recibio "
	    NEXT FIELD generar
               END IF

               WHENEVER ERROR CONTINUE
                  SELECT *
                  INTO   g_seg_modulo.*
                  FROM   seg_modulo
                  WHERE  modulo_cod = "sep"

                  LET archivo_separa = g_seg_modulo.ruta_rescate CLIPPED,"/",
                                       generar CLIPPED

                  LOAD FROM archivo_separa DELIMITER "+"
                  INSERT INTO sep_plano
                      
                  SELECT count(*)
                  INTO   cuantos
                  FROM   sep_plano
                    
                  IF cuantos = 0 THEN
                     DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                     AT 19,2 ATTRIBUTE(REVERSE)
                     SLEEP 2
                     NEXT FIELD generar
                  ELSE
                     EXIT INPUT
                  END IF
               WHENEVER ERROR STOP

	    ON KEY (INTERRUPT)
               ERROR " PROCESO CANCELADO "
               SLEEP 2
               EXIT PROGRAM
	END INPUT
        DISPLAY " PROCESANDO INFORMACION " AT 20,1 ATTRIBUTE(REVERSE)

        SELECT MAX(folio) + 1
        INTO   ultimo_folio
        FROM   glo_folio

        IF ultimo_folio IS NULL THEN
            LET ultimo_folio = 1
        END IF

        INSERT INTO glo_folio VALUES(ultimo_folio)
       
        CALL lee_archivo_plano() #lap

        DISPLAY "TOTAL REGISTROS PROCESADOS     : ",cont AT 10,9        
        DISPLAY "TOTAL REGISTROS NO ENCONTRADOS : ",no_encontrados AT 12,9

        call asigna_globales() 
        call genera_reporte()

        DISPLAY "REPORTE GENERADO EN: " at 14,9
        DISPLAY g_seg_modulo.ruta_listados AT 15,9 ATTRIBUTE(REVERSE)

        PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
        FOR CHAR enter

        CLOSE WINDOW sepc0011
END MAIN

FUNCTION init()
#i-------------
    LET HOY  = TODAY

    SELECT estado ,
           USER
    INTO   s_recibido ,
           usuario 
    FROM   sep_estado_separacion
    WHERE  des_estado = "RECIBIDO"

END FUNCTION

FUNCTION lee_archivo_plano()
#lap------------------------
    DECLARE cur_1 CURSOR FOR
    SELECT  * 
    FROM    sep_plano
   
    LET cont            = 0
    FOREACH cur_1 INTO carga_reg
        DISPLAY " TOTAL REGISTROS PROCESADOS  : ",cont     AT 12,8


              #---Encabezado por lote ---#

        IF carga_reg[1,2] = "01" THEN
            CALL carga_cza() #cc
        END IF

              #---Detalle de nss---#

        IF carga_reg[1,2] = "02" THEN
            LET cont = cont + 1
            CALL carga_det_nss()  #cdn        
        END IF
             
                   #---Sumario de notificacion---#

        IF carga_reg[1,2] = "09" THEN
             LET reg_sum_separa.tipo_registro     = carga_reg[001,002]
             LET reg_sum_separa.total_detalle_02   = carga_reg[003,011]
             LET reg_sum_separa.total_detalle_03   = carga_reg[012,020]

                 INSERT INTO sep_sum_solicitud VALUES(ultimo_folio    ,
                                               reg_sum_separa.* 
                                               )
        END IF
    END FOREACH

END FUNCTION

FUNCTION carga_cza() #cc
#cc---------------------

            LET reg_cza_separa.tipo_registro    = carga_reg[001,002]  
            LET reg_cza_separa.ident_servicio   = carga_reg[003,004] 
            LET reg_cza_separa.ident_operacion  = carga_reg[005,006] 
            LET reg_cza_separa.tipo_ent_origen  = carga_reg[007,008] 
            LET reg_cza_separa.cve_ent_origen   = carga_reg[009,011] 
            LET reg_cza_separa.tipo_ent_destino = carga_reg[012,013] 
            LET reg_cza_separa.cve_ent_destino  = carga_reg[014,016] 
            LET c10_fecha_presenta              = carga_reg[021,022],"/",
                                                  carga_reg[023,024],"/",
                                                  carga_reg[017,020] 
            LET reg_cza_separa.fecha_lote       = c10_fecha_presenta    
            LET reg_cza_separa.consec_lote      = carga_reg[025,027] 
            LET reg_cza_separa.resulta_operacion= carga_reg[028,029]
            LET reg_cza_separa.diag_1           = carga_reg[030,032] 
            LET reg_cza_separa.diag_2           = carga_reg[033,035] 
            LET reg_cza_separa.diag_3           = carga_reg[036,038] 
            LET reg_cza_separa.estado           = s_recibido

                INSERT INTO sep_cza_solicitud VALUES(ultimo_folio,
                                             reg_cza_separa.*
                                            )

                INSERT INTO sep_ctr_archivo VALUES(generar,
                                             ultimo_folio,
                                             reg_cza_separa.fecha_lote,
                                             reg_cza_separa.ident_operacion)

END FUNCTION

FUNCTION carga_det_nss() #cdn
#cdn--------------------

define lidSolicitudSeparacion integer

            LET reg_det_separa.tipo_registro      = carga_reg[001,002]
            LET reg_det_separa.cont_servicio      = carga_reg[003,012]
            LET reg_det_separa.nss                = carga_reg[013,023]
            LET reg_det_separa.rfc                = carga_reg[024,036]
            LET reg_det_separa.curp               = carga_reg[037,054]
            LET reg_det_separa.tipo_ent_admon     = carga_reg[055,056]
            LET reg_det_separa.clave_admon        = carga_reg[057,059]
            LET reg_det_separa.paterno            = carga_reg[060,099]
            LET reg_det_separa.materno            = carga_reg[100,139]
            LET reg_det_separa.nombre             = carga_reg[140,179]
            LET c10_fecha_nac_sep                  = carga_reg[184,185],"/",
                                                     carga_reg[186,187],"/",
                                                     carga_reg[180,183] 
            LET reg_det_separa.fecha_nacimiento   = c10_fecha_nac_sep  
            LET reg_det_separa.ent_nacimiento     = carga_reg[188,189]
            LET reg_det_separa.sexo               = carga_reg[190,190]
            LET reg_det_separa.nombre_procanase   = carga_reg[191,240]
            LET c10_fecha_afilia                  = carga_reg[245,246],"/",
                                                    carga_reg[247,248],"/",
                                                    carga_reg[241,244] 
            LET reg_det_separa.fecha_afiliacion   = c10_fecha_afilia   
            LET c10_fecha_marca                   = carga_reg[253,254],"/",
                                                    carga_reg[255,256],"/",
                                                    carga_reg[249,252] 
            LET reg_det_separa.fecha_marca_infosar        = c10_fecha_marca    
            LET reg_det_separa.diag_confronta     =  carga_reg[257,258]
            LET reg_det_separa.clasifica_separacion= carga_reg[259,259]
            LET reg_det_separa.credito_infonavit  =  carga_reg[260,260]
	         LET reg_det_separa.resulta_operacion  =  carga_reg[261,262]
            LET reg_det_separa.diag_proceso1      =  carga_reg[263,265]
            LET reg_det_separa.diag_proceso2      =  carga_reg[266,268]
            LET reg_det_separa.diag_proceso3      =  carga_reg[269,271]
            LET reg_det_separa.traspaso_previo    =  carga_reg[272,273]

                  SELECT a.correlativo
                  INTO   lidSolicitudSeparacion
                  from   sep_det_reg_sol_reclamante a
                  where  a.n_seguro = reg_det_separa.nss
                  AND    a.estado   = 1
                  AND    a.folio    IS NULL
                  GROUP BY 1

            IF lidSolicitudSeparacion IS NULL THEN 
               LET lidSolicitudSeparacion = 0 
            END IF

            iNSERT INTO sep_det_solicitud VALUES(ultimo_folio,
                                           reg_det_separa.* ,
                                           lidSolicitudSeparacion
                                          )

                  SELECT "OK" 
                  from   sep_det_reg_sol_reclamante 
                  where  n_seguro = reg_det_separa.nss
                  AND    estado   = 1
                  AND    folio    IS NULL

                IF STATUS <> NOTFOUND THEN 

                  UPDATE sep_det_reg_sol_reclamante
                  SET folio  = ultimo_folio ,  
                      estado = s_recibido
                  WHERE  n_seguro = reg_det_separa.nss
                  AND    estado   = 1
                  AND    folio    IS NULL
                ELSE 
                  LET no_encontrados = no_encontrados + 1    
                END IF
                
END FUNCTION

FUNCTION asigna_globales()
#-------------------------   

#== ASIGNACION DE VARIABLES QUE SE UTILIZA EL PROGRAMA SEPREPGRAL.4gl===

   LET  g_tabname      =  "sep_det_solicitud a"
   LET  g_tabname_1    =  "sep_det03_solicitud b"
   LET  g_nombre_prog  =  "SEPC001"
   LET  g_tipo_desc1   =  "REPORTE 1ER. ENVIO OP(27)"

   let g_total_cuentas = cont      

END FUNCTION
