#################################################################################
#Proyecto         => SISTEMA DE AFORES (SAFRE)                                  #
#Propietario      => E.F.P.                                                     #
#Programa RETC826 => RECEPCION DE ARCHIVO OP.13 RETIRO PARCIAL                  #
#Fecha creacion   => 26 FEBRERO 2004.                                           #
#By               => FRANCO ESTEBAN ULLOA VIDELA                                #
#Fecha actualiz.  => 24 DE NOVIEMBRE DEL 2004                                   #
#Actualizacion    => VERONICA LOPEZ SANCHEZ                                     #
#Fecha actualiz.  => 18 DE MAYO DE 2009                                         #
#Actualizacion    => JAVIER GONZALEZ JERONIMO                                   #
#                 => Modificaciones para la adaptacion de la circular 31-11.    #
#Sistema          => RET                                                        #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_dat RECORD
        nom_archivo           CHAR(20)
    END RECORD

    DEFINE gr_edo RECORD
        procesado               LIKE ret_estado.estado_solicitud ,
        recibido                LIKE ret_estado.estado_solicitud ,
        rechazado               LIKE ret_estado.estado_solicitud  
    END RECORD

    DEFINE gr_mov RECORD
        desempleo       SMALLINT,
        matrimonio      SMALLINT
    END RECORD
    
    DEFINE g_reg RECORD #glo #g_reg
        nss                   CHAR(11) ,
        nombres               CHAR(40) ,
        paterno               CHAR(40) ,
        materno               CHAR(40) ,
        tipo_retiro           CHAR(01) ,
        tipo_prestacion       SMALLINT ,
        num_resolucion        INTEGER  ,
        codigo_afore          SMALLINT ,
        diag_cuenta_ind       CHAR(03)
    END RECORD
    
    DEFINE  #glo #reg_param
        g_param_ret           RECORD LIKE seg_modulo.*

    DEFINE reg_cza_lote RECORD
        fecha_operacion       DATE
    END RECORD

    DEFINE #glo #char
        vmarca                CHAR(100),
        vdesmarca             CHAR(100),
        carga_reg             CHAR(360),
        vfecha_aux            CHAR(010),
        vimpt_aux             CHAR(011),
        archivo_retiro        CHAR(200),
        enter                 CHAR(001),
        gc_usuario            CHAR(015),
        cla_sel               CHAR(200)

    DEFINE #glo #smallint
        gs_edo_marca            ,
        gs_marca_causa          ,
        cuantos                 ,
        cont                    ,
        gs_cod_inv              ,
        gs_cod_afore            ,
        gs_tipo_mov             SMALLINT
 
    DEFINE #glo #date
        HOY                     DATE

    DEFINE #glo #integer
        ultimo_folio            ,
        cont_det                INTEGER

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I
	CALL STARTLOG ("RETC826.log")

    CALL init()

    OPEN WINDOW retc8261 AT 4,4 WITH FORM "RETC8261" ATTRIBUTE(BORDER)
    DISPLAY "                             < Ctrl-C > Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC826       CARGA ARCHIVOS DE RETIROS PARCIALES (OP. 13)                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME gr_dat.nom_archivo WITHOUT DEFAULTS

        BEFORE FIELD nom_archivo
            LET gr_dat.nom_archivo = NULL
            CLEAR FORM

        AFTER FIELD nom_archivo
	        IF gr_dat.nom_archivo IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD nom_archivo
            END IF
            
            SELECT "OK"
            FROM   ret_cza_lote
            WHERE  nom_archivo = gr_dat.nom_archivo
            GROUP BY 1
            
            IF STATUS <> NOTFOUND THEN
                ERROR "ARCHIVO YA PROCESADO CON ANTERIORIDAD"
                NEXT FIELD nom_archivo
            END IF
            
            WHENEVER ERROR CONTINUE
            
            SELECT *
            INTO   g_param_ret.*
            FROM   seg_modulo
            WHERE  modulo_cod = "ret"
            
            LET archivo_retiro = g_param_ret.ruta_rescate CLIPPED,"/",
                                 gr_dat.nom_archivo CLIPPED
            
            LOAD FROM archivo_retiro DELIMITER "+"
            INSERT INTO ret_pla_carga2
                      
            SELECT count(*)
            INTO   cuantos
            FROM   ret_pla_carga2
                    
            IF cuantos = 0 THEN
                ERROR "NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"
                NEXT FIELD nom_archivo
            ELSE
                EXIT INPUT
            END IF
            
            WHENEVER ERROR STOP

        ON KEY (INTERRUPT)
            PROMPT  "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    
    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
    CALL primer_paso() #pp
    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR " FOR CHAR enter

    CLOSE WINDOW retc8261

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()
    
    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------
    
    ----- INICIALIZA VARIABLES -----
    
    LET HOY                 = TODAY
    LET gr_mov.matrimonio   = 870
    LET gr_mov.desempleo    = 875
    LET gs_edo_marca        = 0
    LET gs_marca_causa      = 0

    INITIALIZE g_reg.* TO NULL

    ----- CODIGOS AFORES -----
    SELECT codigo_afore   ,
           USER
    INTO   gs_cod_afore   ,
           gc_usuario
    FROM   tab_afore_local    

    SELECT afore_cod
    INTO   gs_cod_inv
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*INVERCAP*"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECHAZADO"

    ----- ULTIMO FOLIO  -----
    SELECT MAX(folio) + 1
    INTO   ultimo_folio
    FROM   glo_folio

    IF ultimo_folio IS NULL THEN
        LET ultimo_folio = 1
    END IF

    INSERT INTO glo_folio 
    VALUES (ultimo_folio)

    ----- DESMARCA DE CUENTA -----
    LET lc_prepare  = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE eje_desmarca FROM lc_prepare
            
    ----- TABLAS TEMPORALES -----
    CALL f_genera_tablas_tmp()

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Ejecuta las instrucciones necesarias para realizar la carga #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE 
        lc_fecha            CHAR(10)

    -- -----------------------------------------------------------------------------

    DECLARE cur_1 CURSOR FOR
    SELECT  * 
    FROM    ret_pla_carga2
    
    LET cont     = 0
    LET cont_det = 0

    FOREACH cur_1 INTO carga_reg
        LET cont = cont + 1
        
        IF carga_reg[1,2] = "01" THEN
            LET lc_fecha = carga_reg[19,20],"/", 
                            carga_reg[21,22],"/",
                            carga_reg[15,18]

            LET reg_cza_lote.fecha_operacion = lc_fecha
            
            SELECT "OK"
            FROM   ret_cza_lote
            WHERE  nom_archivo = gr_dat.nom_archivo
            GROUP BY 1
        
            IF STATUS <> NOTFOUND THEN
                DELETE
                FROM  glo_folio
                WHERE folio = ultimo_folio

                DISPLAY ""  AT 18,2
                PROMPT " ARCHIVO YA PROCESADO CON ANTERIORIDAD.....<ENTER>",
                       " PARA SALIR" FOR CHAR enter
                EXIT PROGRAM
            END IF
        END IF

        DISPLAY " TOTAL REGISTROS PROCESADOS            : ",cont     AT 10,8
        DISPLAY " TOTAL REGISTROS DE DETALLE            : ",cont_det AT 11,8

        IF carga_reg[1,2] = "03" THEN
            CALL f_carga_det()
        END IF 
    
    END FOREACH 
        
    CALL f_actualiza_registros()
    
    DISPLAY "FOLIO NUMERO  : ",ultimo_folio  AT 18,2

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_det : Ejecuta la carga del detalle en la tabla temporal           #
#---------------------------------------------------------------------------#
FUNCTION f_carga_det()

    DEFINE lr_parcial RECORD LIKE ret_parcial.*
    
    DEFINE #loc #char
        des_error             CHAR(100)
        
    DEFINE #loc #smallint
        sw_1                  SMALLINT

    DEFINE #loc #integer
        lr_consec             INTEGER

    -- -----------------------------------------------------------------------------

    LET sw_1     = 0
    LET cont_det = cont_det + 1
    
    DISPLAY " TOTAL REGISTROS DE DETALLE            : ",cont_det AT 11,8

    LET g_reg.nss             = carga_reg[007,017]
    LET g_reg.nombres         = carga_reg[018,057]
    LET g_reg.paterno         = carga_reg[058,097]
    LET g_reg.materno         = carga_reg[098,137]
    LET g_reg.tipo_retiro     = carga_reg[138,138]
    LET g_reg.tipo_prestacion = carga_reg[139,140]
    LET g_reg.num_resolucion  = carga_reg[141,146]
    LET g_reg.codigo_afore    = carga_reg[147,149]
    LET g_reg.diag_cuenta_ind = carga_reg[150,152]
    
    SELECT MAX(consecutivo)
    INTO   lr_consec
    FROM   ret_parcial
    WHERE  nss            = g_reg.nss
    AND    num_resolucion = g_reg.num_resolucion

    SELECT *
    INTO   lr_parcial.*
    FROM   ret_parcial
    WHERE  nss            = g_reg.nss
    AND    num_resolucion = g_reg.num_resolucion
    AND    consecutivo    = lr_consec

    IF (g_reg.diag_cuenta_ind IS NULL) OR (g_reg.diag_cuenta_ind = "   ") THEN
        LET sw_1 = 1
        LET des_error = "NSS: ", g_reg.nss, " ERROR - NSS SIN DIAGNOSTICO DE LA CTA "
    END IF

    IF lr_parcial.tipo_retiro <> g_reg.tipo_retiro THEN
        LET sw_1 = 1
        LET des_error = "NSS: ", g_reg.nss, " ERROR - INCONSISTENCIA TIPO RETIRO ",g_reg.tipo_retiro CLIPPED
    END IF
        
    IF lr_parcial.tipo_prestacion <> g_reg.tipo_prestacion THEN
        LET sw_1 = 1
        LET des_error = "NSS: ", g_reg.nss, " ERROR - INCONSISTENCIA TIPO DE PREST",g_reg.tipo_prestacion CLIPPED
    END IF

    IF lr_parcial.num_resolucion <> g_reg.num_resolucion THEN
        LET sw_1 = 1
        LET des_error = "NSS: ", g_reg.nss, " ERROR - INCONSISTENCIA NUM RESOLUCION ",g_reg.num_resolucion CLIPPED
    END IF
        
    IF g_reg.codigo_afore <> gs_cod_afore THEN
        LET sw_1 = 1
        LET des_error = "NSS: ", g_reg.nss, "ERROR - INCONSISTENCIA COD AFORE ",g_reg.codigo_afore CLIPPED
    END IF

    LET des_error = des_error CLIPPED
    
    -- Avisamos al usuario si existe un error de carga y salimos del programa
    IF sw_1 = 1 THEN
        CALL ERRORLOG(des_error CLIPPED)
        PROMPT des_error FOR CHAR enter

        DELETE
        FROM  glo_folio
        WHERE folio = ultimo_folio

        EXIT PROGRAM
    END IF

    INSERT INTO tmp_ret_parcial2 VALUES (g_reg.*)

END FUNCTION

#---------------------------------------------------------------------------#
# sol_etapa_2 : Ejecuta el cierre automatico de la etapa 2 para el modulo   #
#               de servicios                                                #
#---------------------------------------------------------------------------#
FUNCTION sol_etapa_2( l_nss_afi         ,
                      l_fol_serv        ,
                      l_tipo_serv       ,
                      l_usuario )

#---          Definición de Variables que cacha la Funcion     ---
DEFINE  l_nss_afi               CHAR(11)
DEFINE  l_fol_serv              INTEGER
DEFINE  l_tipo_serv             CHAR(01)
DEFINE  l_usuario               CHAR(10)             
#---  Fin de las Definiciones de Variables que cacha la Funcion ---                                                        
                                                    
DEFINE  l_verif_txt_eta_sol            ,        
        l_procede                      ,
        l_encontro                     ,
        l_consec_sol                   ,
        l_etapa_cod                    ,
        l_resol_linea           SMALLINT
        
DEFINE  l_leyenda_eta2          CHAR(30)

DEFINE  reg_serv     RECORD
        folio_rec    LIKE safre_af:rec_solucion.folio_rec         ,    
        tipo_id      LIKE safre_af:rec_solucion.tipo_id           ,    
        tipo_cod     LIKE safre_af:rec_solucion.tipo_cod          ,    
        motivo_cod   LIKE safre_af:rec_solucion.motivo_cod        ,    
        etapa_cod    LIKE safre_af:rec_solucion.etapa_cod         ,    
        ffin_est     LIKE safre_af:rec_solucion.ffin_est          ,    
        ffin_real    LIKE safre_af:rec_solucion.ffin_real         ,    
        consec_sol   LIKE safre_af:rec_solucion.consec_sol        ,    
        termino_cod  LIKE safre_af:rec_solicitud.termino_cod      ,    
        n_seguro     LIKE safre_af:afi_mae_afiliado.n_seguro           
                     END RECORD        
        
        
    
   #Valores Fijos
   
   LET      l_consec_sol           =         2
   LET      l_etapa_cod            =         2
   LET      l_resol_linea          =         1
   LET      l_leyenda_eta2         =         "SOLICITUD DE RETIRO LIQUIDADO"

   LET      l_encontro             =         0
   LET      l_procede              =         0
                           
                           
   SELECT  COUNT(*)
   INTO    l_encontro
   FROM    rec_solicitud A
   WHERE   A.folio_rec            =        l_fol_serv
   AND     A.tipo_id              =        l_tipo_serv
   AND     A.n_seguro             =        l_nss_afi
   AND     A.termino_cod          =        50          #EN PROCESO DE SOLUCION
   
   
   CASE l_encontro  
      
      WHEN 0  #Registro  -- NO -- ENCONTRADO PARA CERRARLE ETAPA
              
         LET l_procede  = 0 #NO PROCEDE
         RETURN
         
      WHEN 1  #CASO NORMAL
        
         LET l_procede  = 1 #SE PRENDE BANDERA 
       
      OTHERWISE #Registro Duplicado NO  PUEDE CERRARSE LAS ETAPAS
      
         LET l_procede  = 0 #NO PROCEDE  
         RETURN
              
   END CASE
   
   IF  ( l_procede   =   1  )  THEN #REGISTRO SI PROCEDE
                            
                            
      #-- INICIALIZA VARIABLES --#   
            
          INITIALIZE  reg_serv.*  TO   NULL   
                        
      #-- FIN DE INICIALIZACION DE VARIABLES--#
                             
      DECLARE d CURSOR FOR 
        
         SELECT  solucion.folio_rec       ,                       
                 solucion.tipo_id         ,                       
                 solucion.tipo_cod        ,
                 solucion.motivo_cod      ,
                 solucion.etapa_cod       ,
                 solucion.ffin_est        ,
                 solucion.ffin_real       ,
                 solucion.consec_sol      ,                 
                 recsolic.termino_cod     ,
                 recsolic.n_seguro
         FROM    rec_solucion solucion    , rec_solicitud recsolic
         WHERE   solucion.folio_rec  > 0
         AND     recsolic.folio_rec       =       l_fol_serv
         AND     recsolic.tipo_id         =       l_tipo_serv
         AND     recsolic.folio_rec       =       solucion.folio_rec
         AND     recsolic.tipo_id         =       solucion.tipo_id
         AND     solucion.ffin_real  IS NULL
         AND     recsolic.termino_cod     =   50 #EN PROCESO DE SOLUCION      
         AND     solucion.consec_sol      =   2  #Esto es para cerrarle la 2da
                                                 #etapa y asegurarnos que tenga
                                                 #2da Etapa.                                  
      FOREACH d INTO reg_serv.*   
                                                                                                
         UPDATE  safre_af:rec_solicitud                                                           
         SET     termino_cod       =   90 #SOLUCIONADO
         WHERE   folio_rec         =   reg_serv.folio_rec 
         AND     tipo_id           =   reg_serv.tipo_id
         AND     n_seguro          =   reg_serv.n_seguro
         
         UPDATE  safre_af:rec_solucion                                        
         SET     ffin_real         =   TODAY                                  
         WHERE   folio_rec         =   reg_serv.folio_rec                                                                   
         AND     tipo_id           =   reg_serv.tipo_id                       
         AND     etapa_cod         =   l_etapa_cod   #2                                      
         AND     consec_sol        =   l_consec_sol  #2
         
         
         
         #Verifica si existe el registro antes de INSERTARSE 
         
         LET l_verif_txt_eta_sol   =    0
         
         SELECT  COUNT(*)
         INTO    l_verif_txt_eta_sol
         FROM    rec_txt_etapa_sol B
         WHERE   B.folio_rec         =   reg_serv.folio_rec         
         AND     B.tipo_id           =   reg_serv.tipo_id 
         AND     B.consec_sol        =   l_consec_sol    #2   
         AND     B.etapa_cod         =   l_etapa_cod     #2
         
         IF ( l_verif_txt_eta_sol = 0 ) THEN #-- NO EXISTE REG -- SE PUEDE INSERTAR               
                                                                                                
            INSERT INTO safre_af:rec_txt_etapa_sol VALUES ( reg_serv.folio_rec  ,#folio_rec       
                                                            reg_serv.tipo_id    ,#tipo_id         
                                                            l_consec_sol        ,#consec_sol      
                                                            l_etapa_cod         ,#etapa_cod       
                                                            l_resol_linea       ,#resol_linea     
                                                            l_leyenda_eta2      ,#resol_eta       
                                                            l_usuario           ,#usuario         
                                                            TODAY               #factualiza       
                                                                                                  
                                                            )                                     
                                                                                                
         ELSE #Ya existe el registro --NO SE INSERTA                                            
                                                                                                
                                                                                                
                                                                                                
         END IF                                                                                 
                                                                                                
                     
                     
      END FOREACH       
      
    
   END IF #FIN DE  VALIDACION DE REGISTRO PROCEDIDO ó NO 
   
   #---INICIALIZA VARIABLES DE SALIDA  sol_etapa_2 ---#
   
   
   LET      l_consec_sol           =         0
   LET      l_etapa_cod            =         0
   LET      l_resol_linea          =         0
   LET      l_leyenda_eta2         =         NULL
   LET      l_encontro             =         0
   LET      l_procede              =         0
   LET      l_verif_txt_eta_sol    =         0
   
   #---FIN DE INICIALIZACION DE VARIABLES ---#
   
    
                               
    
END FUNCTION


#---------------------------------------------------------------------------#
# f_actualiza_registros : Inserta los registros a las tablas fisicas        #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_registros()

    DEFINE
        li_consec_tmp          ,
        li_consecutivo         INTEGER

    DEFINE
        ld_pago_desempleo      DECIMAL(10,2)

    DEFINE lc_folio_sol LIKE ret_parcial.folio_solicitud

    DEFINE lr_etapa_2 RECORD
        nss                 CHAR(11) ,
        folio_serv          INTEGER  ,
        tipo_serv           CHAR(01) ,
        usuario             CHAR(15)  
    END RECORD

    -- -----------------------------------------------------------------------------
    
    LET lr_etapa_2.tipo_serv    = "S"
    LET lr_etapa_2.usuario      = gc_usuario
        
    INSERT INTO ret_cza_lote
    VALUES (ultimo_folio                 , -- folio
            reg_cza_lote.fecha_operacion , 
            HOY                          , -- fecha_valor_trans
            gr_dat.nom_archivo           , 
            HOY                          , -- fecha_carga
            cuantos                      , -- tot_registros
            gr_edo.recibido                -- estado_lote
           )

    DECLARE cur_act CURSOR FOR
    SELECT * 
    FROM   tmp_ret_parcial2

    FOREACH cur_act INTO g_reg.*
        
	    LET ld_pago_desempleo = 0
	    
	    SELECT MAX(consecutivo)
	    INTO   li_consec_tmp
	    FROM   ret_parcial
        WHERE  nss              = g_reg.nss
        AND    num_resolucion   = g_reg.num_resolucion
        AND    estado_solicitud = gr_edo.procesado
        
        UPDATE ret_parcial
        SET    diag_cuenta_ind  = g_reg.diag_cuenta_ind
        WHERE  nss              = g_reg.nss
        AND    num_resolucion   = g_reg.num_resolucion
	    AND    consecutivo      = li_consec_tmp
	    AND    estado_solicitud = gr_edo.procesado
        
        SELECT folio_solicitud  ,
               pago_desempleo   ,
               consecutivo
        INTO   lc_folio_sol      ,
               ld_pago_desempleo ,
               li_consecutivo
        FROM   ret_parcial
        WHERE  nss              = g_reg.nss
        AND    num_resolucion   = g_reg.num_resolucion
	    AND    consecutivo      = li_consec_tmp
	    AND    estado_solicitud = gr_edo.procesado

        -- Si es Invercap, se ejecuta el cierre automatico de la etapa 2
        IF gs_cod_afore = gs_cod_inv THEN
            
            LET lr_etapa_2.nss         = g_reg.nss
            LET lr_etapa_2.folio_serv  = lc_folio_sol

            CALL sol_etapa_2(lr_etapa_2.*)
        END IF    
	    
	    IF ld_pago_desempleo IS NULL THEN
	        LET ld_pago_desempleo = 0
	    END IF
        
        INSERT INTO ret_parcial_tx
        VALUES (g_reg.nss        , -- nss
                li_consecutivo   , -- consecutivo
                ultimo_folio     , -- folio
                "01-01-0001"     , -- fecha_valuacion
                "01-01-0001"     , -- fecha_pago
                0                , -- impt_ret_97
                0                , -- impt_ces_vej
                0                , -- impt_cuo_soc
                0                , -- impt_tot_sub_rcv
                0                , -- saldo_cta_individual
                ld_pago_desempleo  -- pago_desempleo
               )
          
        -- Se verifica si el registro es un rechazo
        SELECT "OK"
        FROM   tab_diag_procesar_disp
        WHERE  diag_procesar    = g_reg.diag_cuenta_ind 
        AND    id_aceptado      = 1
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
            -- Si es un rechazo entonces se desmarca la cuenta y se actualiza su estado
            IF g_reg.tipo_prestacion = 6 THEN
                LET gs_tipo_mov = gr_mov.desempleo
            ELSE
                LET gs_tipo_mov = gr_mov.matrimonio
            END IF
   
            EXECUTE eje_desmarca USING g_reg.nss        ,
                                       gs_tipo_mov      ,
                                       li_consecutivo   ,
                                       gs_edo_marca     ,
                                       gs_marca_causa   ,
                                       gc_usuario
            
            UPDATE ret_parcial
            SET    estado_solicitud = gr_edo.rechazado
            WHERE  nss              = g_reg.nss
            AND    num_resolucion   = g_reg.num_resolucion
	        AND    consecutivo      = li_consecutivo
        END IF
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_tablas_tmp : Genera las tablas temporales que se usaran durante  #
#                       el proceso                                          #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tablas_tmp()

    CREATE TEMP TABLE ret_pla_carga2
        (n_registros          CHAR(360))

    CREATE TEMP TABLE tmp_ret_parcial2
        (
         nss                  CHAR(11) ,
         nombres              CHAR(40) ,
         paterno              CHAR(40) ,
         materno              CHAR(40) ,
         tipo_retiro          CHAR(1)  ,
         tipo_prestacion      SMALLINT ,
         num_resolucion       INTEGER  ,
         codigo_afore         SMALLINT ,
         diag_cuenta_ind      SMALLINT
        )

END FUNCTION

