#######################################################################
#Proyecto          => AFORES ( MEXICO )                               #
#Propietario       => E.F.P.                                          #
#Programa Llamado  => TAAB0021 Provisiona Saldos Recibidos            #
#Programa Llamador => TAAB002                                         #
#Fecha             => 31 DE ENERO DE 2001                             #
#Autor             => MAURO MUNIZ CABALLERO                           #
#Fecha actualiza   => 19 DE JULIO DE 2004                             #
#Por               => MAURO MUNIZ CABALLERO.                          #
#   Adecuaciones para circular 28-8 y participaciones                 #
#Fecha actualiza   => 3 DE DICIEMBRE DE 2004                          #
#Por               => MAURO MUNIZ CABALLERO.                          #
#   Adecuaciones para circular 28-9 ( multisiefores )                 #
#Fecha actualiza   => 28 DE FEBRERO DE 2008                           #
#Por               => JOSUE LISANDRO HUERTA SIERRA                    #
#   Adecuaciones para multi-siefore (circular 69-2 )                  #
#Sistema           => TRA                                             #
#EFPS-193          => JCPV  01/03/2012 Optimizar Provision            #
#Req:1038          => JCPV  15/11/2012 Apertura de Cuentas Certificada#
#Req:1361 				 => FSR Se cambia a dos decimales el monto					#
#######################################################################

DATABASE safre_af

GLOBALS

    DEFINE
        cza RECORD LIKE taa_cza_recepcion.*,
        rcv RECORD LIKE taa_rcv_recepcion.*,
        viv RECORD LIKE taa_viv_recepcion.*,
        sum RECORD LIKE taa_sum_recepcion.*,
        reg_prov RECORD LIKE safre_af:dis_provision.*

    DEFINE
        dia_liq  ,
        HOY      DATE 

    DEFINE 
        cza_id_op CHAR(02) ,
        aportante CHAR(06) ,
        HORA      CHAR(10) ,
        usuario   CHAR(08) ,
        comma     CHAR(50)

    DEFINE
        vfolio          INTEGER

    DEFINE g_lista CHAR(100)

    DEFINE g_param_taa RECORD LIKE seg_modulo.*
    
    DEFINE v_desmarca        CHAR(300),
           vmarca            SMALLINT ,
           vcorrelativo      INTEGER
    DEFINE reg_graba2        INTEGER
    DEFINE cuantos           INTEGER
END GLOBALS

MAIN

    LET vfolio = ARG_VAL(1)
    LET HOY    = TODAY
    LET HORA   = TIME
    CALL STARTLOG("TAAB0021.log")     
    DISPLAY "Inicia TAAB0021.4gi Folio ", 
             vfolio USING "######&", ' ', 
             HOY USING "DD/MM/YYYY", ' A LAS: ', HORA
    CALL inicio()
    CALL proceso_principal()
    CALL rpt_vol()
    LET HOY    = TODAY
    LET HORA   = TIME
    DISPLAY "Registros Grabados ", reg_graba2 USING "##,###,##&",
            'De: ',cuantos
    DISPLAY "Termina prb0021.4gi ", HOY USING "DD/MM/YYYY", ' A LAS: ', HORA

END MAIN

FUNCTION inicio()
#i---------------

    LET HORA = TIME
    LET HOY  = TODAY
    LET reg_graba2    = 0
    LET cuantos = 0
    SELECT *, USER
    INTO   g_param_taa.*, usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'

    LET g_lista    = g_param_taa.ruta_listados CLIPPED, "/",usuario CLIPPED,
                     ".rpt_vol.",HOY using "DDMMYY"
                  
    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "                  
    SELECT count(*)
    INTO cuantos
     FROM safre_tmp:recep
    WHERE folio = vfolio
  
END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    DEFINE 
        bandera SMALLINT,
        vnss    CHAR(11)

    DEFINE
        TODOS1               INTEGER

    DEFINE reg_recep RECORD
        folio                INTEGER      ,
        cont_servicio        DECIMAL(10,0),
        ident_operacion      CHAR(02)     ,
        cve_ced_cuenta       CHAR(3)      ,
        tipo_traspaso        SMALLINT     ,
        fecha_mov_banxico    DATE         ,
        fecha_presentacion   DATE         ,
        nss                  CHAR(11)     ,
        curp                 CHAR(18)     ,
        cve_subcta           CHAR(2)      ,
        prctj_subc           DECIMAL(16,6),
        saldo_subc           DECIMAL(16,6),
        no_tot_acc           DECIMAL(16,6),
        siefore              CHAR(8)      ,
        precio_acc           DECIMAL(16,6),
        subcuenta            SMALLINT     ,
        codigo_siefore       SMALLINT     ,
        porcentaje           DECIMAL(16,6),
        precio_ced           DECIMAL(16,6),
        precio_own           DECIMAL(16,6),
        monto_pesos          DECIMAL(18,2),#CPL-1361
        monto_accs           DECIMAL(18,6)
    END RECORD
 
    DEFINE opc SMALLINT
    DEFINE xid_aportante CHAR(10)
    DEFINE gfecha_valor DATE
    DEFINE xcurp        CHAR(18)

---- 1038 ->
     SELECT cr.*
     INTO   cza.*
     FROM   taa_cza_recepcion cr
     WHERE  folio = vfolio
     
     IF cza.ident_operacion = '09'  THEN
       CALL traspasa_maestro()
     END IF
---- 1038 <-     
     
    DECLARE cur_prov1 CURSOR FOR 
    SELECT *
    FROM safre_tmp:recep
    WHERE folio = vfolio
    FOREACH cur_prov1 
     INTO reg_recep.*
     SELECT tt.id_aportante
      INTO xid_aportante            
      FROM tab_tipo_traspaso tt
     WHERE tt.tipo_traspaso = reg_recep.tipo_traspaso 
       AND tt.modulo_cod    = 'taa'
       AND tt.id_opera      = '09'                   

     LET xid_aportante       =  xid_aportante CLIPPED,
                                reg_recep.cve_ced_cuenta 
     IF  reg_recep.cve_subcta = 36
     AND reg_recep.subcuenta  = 31  THEN
        LET  xid_aportante    = "APOR-TRAB"
      ELSE
       IF  reg_recep.cve_subcta = 37
       AND reg_recep.subcuenta  = 31  THEN
          LET  xid_aportante    = "APOR-PAT "
       END IF
     END IF                                                                
     IF  reg_recep.subcuenta = 4  
     OR  reg_recep.subcuenta = 8 
     OR  reg_recep.subcuenta = 14
     OR  reg_recep.subcuenta = 35 THEN
        LET gfecha_valor        =  MDY(MONTH(reg_recep.fecha_mov_banxico),
                                   1,YEAR(reg_recep.fecha_mov_banxico))
        LET xcurp               =  reg_recep.curp
      ELSE
        LET gfecha_valor        = reg_recep.fecha_mov_banxico
        LET xcurp               = ' '
     END IF
     LET reg_prov.tipo_movimiento     = 1
     LET reg_prov.subcuenta           = reg_recep.subcuenta
     LET reg_prov.siefore             = reg_recep.codigo_siefore
     LET reg_prov.folio               = reg_recep.folio
     LET reg_prov.consecutivo_lote    = reg_recep.cont_servicio
     LET reg_prov.nss                 = reg_recep.nss
     LET reg_prov.curp                = xcurp
     LET reg_prov.folio_sua           = 0
     LET reg_prov.fecha_pago          = reg_recep.fecha_mov_banxico
     LET reg_prov.fecha_valor         = gfecha_valor
     LET reg_prov.fecha_conversion    = reg_recep.fecha_mov_banxico
     LET reg_prov.monto_en_pesos      = reg_recep.monto_pesos
     LET reg_prov.monto_en_acciones   = reg_recep.monto_accs
     LET reg_prov.precio_accion       = reg_recep.precio_own
     LET reg_prov.dias_cotizados      = 0
     LET reg_prov.sucursal            = 0
     LET reg_prov.id_aportante        = xid_aportante
     LET reg_prov.estado              = 5
     LET reg_prov.fecha_proceso       = TODAY
     LET reg_prov.usuario             = usuario
     LET reg_prov.fecha_archivo       = reg_recep.fecha_presentacion
     LET reg_prov.etiqueta            = 0

     IF   reg_prov.monto_en_pesos     > 0  THEN
       INSERT INTO safre_af:dis_provision VALUES(reg_prov.*)  #TEMP
       LET  reg_graba2  = reg_graba2 + 1
       IF reg_graba2  MOD 10000 = 0 THEN
         LET HORA  = TIME
         DISPLAY "Procesados: ", reg_graba2 ,  ' ', HORA, ' De: ', cuantos 
       END IF
     END IF
     
            
     LET bandera = 0

     SELECT cr.*
     INTO   cza.*
     FROM   taa_cza_recepcion cr
     WHERE  folio = vfolio
 
     IF reg_recep.ident_operacion = '12' THEN
       DECLARE cur_desmarca CURSOR FOR
	     SELECT mc.marca_cod, mc.correlativo
	     FROM cta_act_marca mc
	     WHERE mc.nss       = reg_recep.nss
	     AND mc.marca_cod = 140
	
	     FOREACH cur_desmarca INTO vmarca, vcorrelativo
	      CALL desmarca_cuenta ( reg_recep.nss, vmarca, usuario,    #TEMP
	                             vcorrelativo)
	    END FOREACH
	   END IF

    
   END FOREACH       #cur_prov1  

   CALL actualiza_historicos()   #TEMP

END FUNCTION       #proceso_principal

---- 1038 ->  Apertura Cuentas aun no aperturadas
FUNCTION traspasa_maestro()
    
    LET comma = "fglgo TAAB010.4gi ", 1, vfolio
    DISPLAY comma
    RUN comma
    
END FUNCTION       #traspasa_maestro()  
---- 1038 <-

FUNCTION actualiza_historicos()

    UPDATE taa_cza_recepcion
    SET    estado = 2
    WHERE  folio = vfolio
    AND    tipo_registro = "01"
    AND    estado = 1

    UPDATE taa_rcv_recepcion
    SET    estado = 2
    WHERE  folio = vfolio
    AND    tipo_registro = "25"
    AND    estado = 1

    UPDATE taa_viv_recepcion
    SET    estado = 2
    WHERE  folio = vfolio
    AND    tipo_registro = "25"
    AND    estado = 1

    UPDATE taa_sum_recepcion
    SET    estado = 2
    WHERE  folio= vfolio
    AND    tipo_registro = "09"
    AND    estado = 1

    UPDATE taa_recepcion_af
    SET    estado = 2
    WHERE  folio= vfolio
    AND    estado = 1

END FUNCTION

FUNCTION rpt_vol()
#rv---------------

    DEFINE rpt_vol RECORD
         folio                INTEGER      ,
         nss                  CHAR(11)     ,
         subcuenta            CHAR(2)      ,
         monto_accs           DECIMAL(18,6)
    END RECORD

    DEFINE vfecha_vol DATE
    DEFINE vfecha_pat DATE
    DEFINE vfecha_ven DATE


    LET vfecha_vol = ''
    LET vfecha_ven = ''
    LET vfecha_pat = ''    

    DECLARE cur_vol CURSOR FOR
    SELECT folio       ,
           nss         ,
           subcuenta  ,
           monto_accs
    FROM   safre_tmp:recep
    WHERE  subcuenta in('3','10')

    START REPORT listado TO g_lista

    FOREACH cur_vol INTO rpt_vol.*

        IF rpt_vol.monto_accs <> 0 OR
           rpt_vol.monto_accs IS NOT NULL THEN
            IF rpt_vol.subcuenta = '3' THEN
            	 DECLARE csr_trr1 CURSOR FOR
                SELECT fecha_vol_ven
                FROM   safre_af:taa_rcv_recepcion
                WHERE  folio = rpt_vol.folio
                AND    nss   = rpt_vol.nss
               FOREACH csr_trr1 
                	INTO  vfecha_ven
                  IF    vfecha_ven = '01/01/0001' THEN
                  	  LET  vfecha_vol  =  vfecha_ven
                   ELSE                   	                  	                   
                    IF    vfecha_ven IS NULL  THEN
                  	   LET  vfecha_vol  =  vfecha_ven
                    END IF                    
                  END IF
                  IF  vfecha_vol IS NULL            	 
                  OR  vfecha_vol = '01/01/0001'  THEN
                    OUTPUT TO REPORT listado(rpt_vol.*)
                    EXIT FOREACH
                  END IF
                 END FOREACH                   
             ELSE
            	 DECLARE csr_trr2 CURSOR FOR
                SELECT fecha_vol_pat
                FROM   safre_af:taa_rcv_recepcion
                WHERE  folio = rpt_vol.folio
                AND    nss   = rpt_vol.nss
               FOREACH csr_trr2
                	INTO  vfecha_pat 
                  IF    vfecha_pat = '01/01/0001' THEN
                  	  LET  vfecha_vol  =  vfecha_pat
                   ELSE
                   	IF  vfecha_pat  IS NULL   THEN
                   		 LET  vfecha_vol  =  vfecha_pat
                 	  END IF
                  END IF
                  IF  vfecha_vol = '01/01/0001' 
                  OR  vfecha_vol  IS  NULL  THEN
                    OUTPUT TO REPORT listado(rpt_vol.*)
                    EXIT FOREACH
                  END IF
               END FOREACH                                 
            END IF
        END IF
         

        LET vfecha_vol = ''
        LET vfecha_ven = ''
        LET vfecha_pat = ''

    END FOREACH

    FINISH REPORT listado

    UPDATE taa_ctr_traspaso         
       SET fin_provision = CURRENT
     WHERE folio = vfolio

END FUNCTION

REPORT listado(rpt_vol)
#l---------------------

    DEFINE rpt_vol RECORD
         folio                INTEGER      ,
         nss                  CHAR(11)     ,
         subcuenta            CHAR(2)      ,
         monto_accs           DECIMAL(18,6)
    END RECORD

    OUTPUT
        PAGE LENGTH 60
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER

    PRINT COLUMN 5,"REPORTE NSS CON APORTACIONES VOLUNTARIAS CON FECHA ERRONEA"
    PRINT
    PRINT COLUMN 5,"FOLIO     NSS          SUBCTA "
    PRINT

    ON EVERY ROW
    PRINT COLUMN 5,rpt_vol.folio USING "&&&&&&&",
          COLUMN 15, rpt_vol.nss,
          COLUMN 28, rpt_vol.subcuenta

END REPORT

FUNCTION desmarca_cuenta (vnss, vmarca, vusuario, vcorrelativo)
#dc------------------------------------------------------------

    DEFINE
        vnss           CHAR(11),
        vmarca         SMALLINT,
        vusuario       CHAR(08),
        vcorrelativo   INTEGER,
        vestado_marca  SMALLINT,
        vmarca_causa   SMALLINT

    LET vestado_marca = 0
    LET vmarca_causa  = 0

    PREPARE eje_desmarca FROM v_desmarca

    EXECUTE eje_desmarca
    USING vnss,
          vmarca,
          vcorrelativo,
          vestado_marca,
          vmarca_causa,
          vusuario

END FUNCTION

