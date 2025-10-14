##############################################################################
#Owner             => E.F.P.
#Programa TRAC008  => REVERSA LIQUIDACION DE ICEFAS           
#Fecha creacion    => 20 DE ABRIL DE 1998
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 08 DE MARZO DEL 2011
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af

GLOBALS

DEFINE  g_fecha_conversion                DATE
DEFINE  g_sql_desmarca                    CHAR(500)
DEFINE  vv_corr                           INTEGER
DEFINE  m1 LIKE dis_cuenta.monto_en_pesos

DEFINE  uu CHAR(008)
DEFINE  u1 CHAR(4)
DEFINE  u2 CHAR(2)
DEFINE  u3 CHAR(2)

DEFINE  reg_cta  RECORD  LIKE dis_cuenta.*
DEFINE  v_saldo_sar_92 DECIMAL(16,6)       
DEFINE  reg_llave RECORD 
        n_seguro       LIKE tra_det_trasp_sal.n_seguro,
        n_seguro_ent   LIKE tra_det_trasp_sal.n_seguro_ent,
        rfc_ent        LIKE tra_det_trasp_sal.rfc_ent,
        cve_ced_cuenta LIKE tra_det_trasp_sal.cve_ced_cuenta,
        nro_ctrl_icefa LIKE tra_det_trasp_sal.nro_ctrl_icefa
                  END RECORD

DEFINE  reg_marca RECORD LIKE safre_af:dis_cuenta.* 
        
DEFINE  reg RECORD #reg
        folio                 INTEGER ,
        sar_92                CHAR(1) ,
        viv_92                CHAR(1) ,
        descripcion           CHAR(30)
            END RECORD

DEFINE g_reg RECORD #g_reg
       n_seguro             LIKE tra_det_trasp_sal.n_seguro        ,
       cve_ced_cuenta       LIKE tra_det_trasp_sal.cve_ced_cuenta  ,
       nro_ctrl_icefa       LIKE tra_det_trasp_sal.nro_ctrl_icefa
             END RECORD

DEFINE reg_4 RECORD #glo #reg_4
       aceptada                      ,
       devuelta                      ,
       no_atendida                   ,
       rechazada                     ,
       complementarios               ,
       provisionada                  , 
       liquidada             SMALLINT
             END RECORD

DEFINE #date
       HOY                   DATE

DEFINE #char
       enter                 CHAR(1)

DEFINE #smallint
       s_status_liq          SMALLINT
    
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP ,
        PROMPT LINE LAST ,
        ACCEPT KEY CONTROL-I
        
    CALL init()
    
    OPEN WINDOW trac0081 AT 4,4 WITH FORM "TRAC0081" ATTRIBUTE(BORDER)
    DISPLAY " TRAC008            REVERSA LIQUIDACION ICEFA-AFORE IMSS                       " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET reg.folio = NULL
    
    INPUT BY NAME reg.folio  ,
                  reg.sar_92 ,
                  reg.viv_92 WITHOUT DEFAULTS

        AFTER FIELD folio
          
            IF reg.folio = "" THEN
                ERROR "FOLIO NO PUEDE SER NULO"
                NEXT FIELD FOLIO
            END IF

        AFTER FIELD sar_92
          
            IF reg.sar_92 <> "X" THEN
                ERROR "SOLO SE PERMITE MARCAR CON 'X' "
                NEXT FIELD sar_92
            END IF

        AFTER FIELD viv_92
          
            IF reg.viv_92 <> "X" THEN
                ERROR "SOLO SE PERMITE MARCAR CON 'X' "
                NEXT FIELD viv_92
            END IF

        ON KEY (ESC)  
          
            IF reg.sar_92 IS NULL AND reg.viv_92 IS NULL THEN
              
                ERROR "NO SE PUEDE REVERSAR SIN MARCAR UNA DE LAS OPCIONES "
                NEXT FIELD folio
                
            END IF
            
            WHILE TRUE
              
                PROMPT " ESTA SEGURO S/N " FOR CHAR enter
                IF enter MATCHES "[SsNn]" THEN
                    IF enter MATCHES "[Ss]" THEN
                        EXIT WHILE
                    ELSE
                        PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR "
                        FOR CHAR enter
                        EXIT PROGRAM
                    END IF
                END IF
            END WHILE

            DISPLAY "PROCESANDO INFORMACION" AT 17,2 ATTRIBUTE(REVERSE)
            SLEEP 2


 LET    g_fecha_conversion   =       NULL
 
 SELECT a.fecha_conversion
 INTO   g_fecha_conversion
 FROM   dis_cuenta a
 WHERE  a.folio              =       reg.folio
 GROUP BY 1
 
 CALL separa_nss(reg.folio)

 LET    reg_4.provisionada   =       NULL               
               
 SELECT estado
 INTO   reg_4.provisionada
 FROM   tra_status
 WHERE  des_estado = "PROVISIONADA"


 IF reg.sar_92 = "X" THEN
  
  DECLARE cur_marca CURSOR FOR 

	  SELECT a.* 
	  FROM   dis_cuenta a
	  WHERE  a.folio              =      reg.folio
	  AND    a.subcuenta          =      7#SAR     
    AND    a.tipo_movimiento IN (1,4)

  FOREACH cur_marca INTO   reg_marca.*

     IF reg_marca.tipo_movimiento = 1 THEN
       
        SELECT a.n_seguro                 ,
               a.n_seguro_ent             ,
               a.rfc_ent                  ,
               a.cve_ced_cuenta           ,
               a.nro_ctrl_icefa           ,
	             a.ident_lote_solic[6,13]
        INTO   reg_llave.*                ,
	             uu
        FROM   tra_det_trasp_sal a
        WHERE  a.folio             = reg_marca.folio
        AND    a.cont_servicio     = reg_marca.consecutivo_lote
        
     END IF
     
     IF reg_marca.tipo_movimiento = 4 THEN
       
          SELECT a.n_seguro              ,
	              a.n_seguro_ent_ced       ,
	              a.rfc_ent_ced            ,
	              a.cve_ced_cuenta         ,
	              a.nro_ctrl_icefa         ,
	              a.ident_lote_solic[6,13]
	        INTO  reg_llave.*              ,
	              uu
	        FROM  tra_det_trasp_int a
	        WHERE a.folio         = reg_marca.folio
	        AND   a.cont_servicio = reg_marca.consecutivo_lote
	   
     END IF                                                                  
     
     LET u1 = uu[1,4]
     LET u2 = uu[5,6]
     LET u3 = uu[7,8]
              
     LET    vv_corr              =  NULL
     
     SELECT A.correlativo
     INTO   vv_corr
     FROM   tra_mae_icefa A 
     WHERE  A.n_seguro           = reg_llave.n_seguro
     AND    A.nss                = reg_llave.n_seguro_ent
     AND    A.rfc                = reg_llave.rfc_ent
     AND    A.icefa_cod          = reg_llave.cve_ced_cuenta
     AND    A.nro_int_cta        = reg_llave.nro_ctrl_icefa
     AND    YEAR(A.fecha_genera) =  u1
     AND    MONTH(A.fecha_genera)=  u2
     AND    DAY(A.fecha_genera)  =  u3
     AND    A.status in (7,8,17)
     
     SELECT "OK"
     FROM   v_viv A
     WHERE  A.n_seguro           = reg_marca.nss
     AND    A.cont_servicio      = reg_marca.consecutivo_lote
     
     IF STATUS = NOTFOUND THEN
      
        CALL f_marca_cuenta(reg_marca.nss,250,vv_corr)
        
     END IF

 END FOREACH

    DELETE 
    FROM   dis_cuenta
    WHERE  folio     = reg.folio
    AND    subcuenta = 7
    
    UPDATE dis_provision
    SET    estado = reg_4.provisionada
    WHERE  folio     = reg.folio
    AND    subcuenta = 7
    
    UPDATE tra_his_dep_icefa
    SET    estado            = reg_4.provisionada ,
           fecha_liquidacion = ""
    WHERE  folio     = reg.folio
    AND    subcuenta = 7
    
		CALL actualiza_edo("S")

 END IF

  IF reg.viv_92 = "X" THEN


  DECLARE cur_marca_viv CURSOR FOR 
    
	  SELECT a.* 
	  FROM   dis_cuenta a
	  WHERE  a.folio             = reg.folio
	  AND    a.subcuenta         = 8      
    AND    a.tipo_movimiento IN (1,4)

  FOREACH cur_marca_viv INTO   reg_marca.*

     IF reg_marca.tipo_movimiento = 1 THEN
       
        SELECT a.n_seguro                , 
               a.n_seguro_ent            ,
               a.rfc_ent                 ,
               a.cve_ced_cuenta          ,
               a.nro_ctrl_icefa          ,
	             a.ident_lote_solic[6,13]
        INTO   reg_llave.*               ,
	             uu
        FROM   tra_det_trasp_sal a
        WHERE  a.folio            = reg_marca.folio
        AND    a.cont_servicio    = reg_marca.consecutivo_lote
        
     END IF

     IF reg_marca.tipo_movimiento = 4 THEN
       
        SELECT a.n_seguro          ,
	             a.n_seguro_ent_ced  ,
	             a.rfc_ent_ced       ,
	             a.cve_ced_cuenta,
	             a.nro_ctrl_icefa ,
	             a.ident_lote_solic[6,13]
	      INTO   reg_llave.*,
	             uu
	      FROM   tra_det_trasp_int a
	      WHERE  a.folio         = reg_marca.folio
	      AND    a.cont_servicio = reg_marca.consecutivo_lote
	      
     END IF                                                                  

     LET u1 = uu[1,4]
     LET u2 = uu[5,6]
     LET u3 = uu[7,8]

     LET    vv_corr                =      NULL
                                          
     SELECT A.correlativo                 
     INTO   vv_corr                       
     FROM   tra_mae_icefa A               
     WHERE  A.n_seguro             =      reg_llave.n_seguro
     AND    A.nss                  =      reg_llave.n_seguro_ent
     AND    A.rfc                  =      reg_llave.rfc_ent
     AND    A.icefa_cod            =      reg_llave.cve_ced_cuenta
     AND    A.nro_int_cta          =      reg_llave.nro_ctrl_icefa
     AND    YEAR(A.fecha_genera)   =      u1
     AND    MONTH(A.fecha_genera)  =      u2
     AND    DAY(A.fecha_genera)    =      u3
     AND    A.status in (7,8,17)

     SELECT "OK"
     FROM   v_viv A
     WHERE  A.n_seguro             = reg_marca.nss
     AND    A.cont_servicio        = reg_marca.consecutivo_lote

     IF STATUS <> NOTFOUND THEN
      
        CALL f_marca_cuenta(reg_marca.nss,250,vv_corr)
        
     END IF

 END FOREACH


    DELETE 
    FROM  dis_cuenta
    WHERE  folio     = reg.folio
    AND    subcuenta = 8
    
    UPDATE dis_provision
    SET    estado = reg_4.provisionada
    WHERE  folio     = reg.folio
    AND    subcuenta = 8
    
    UPDATE tra_his_dep_icefa
    SET    estado            = reg_4.provisionada ,
           fecha_liquidacion = ""
    WHERE  folio     = reg.folio
    AND    subcuenta = 8

	CALL actualiza_edo("V")
	
  END IF
              
     WHENEVER ERROR STOP
     
     LET reg.folio       =       NULL
     LET reg.sar_92      =       NULL
     LET reg.viv_92      =       NULL
     LET reg.descripcion =       "PROCESO FINALIZADO" 
     
     DISPLAY "                      " AT 17,2 ATTRIBUTE(NORMAL)
     DISPLAY BY NAME reg.folio
     DISPLAY BY NAME reg.sar_92
     DISPLAY BY NAME reg.viv_92
     DISPLAY BY NAME reg.descripcion
          
  END INPUT
  
  CLOSE WINDOW trac0081
    
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    LET g_sql_desmarca = 'EXECUTE PROCEDURE reversa_desmarca(?,?,?,?)'
    PREPARE  sql_desmarca FROM g_sql_desmarca
     
    LET g_sql_desmarca = g_sql_desmarca  CLIPPED

END FUNCTION

FUNCTION actualiza_edo(tipo)
#ae-----------------------

     DEFINE tipo char(001)
     DEFINE bandera smallint

     LET bandera = 0

     IF tipo = "S" THEN

        SELECT "OK" 
        FROM   dis_cuenta
        WHERE  folio = reg.folio
        AND    subcuenta = 8
        GROUP BY 1
        
        IF STATUS <> NOTFOUND THEN
           LET bandera = 1
        END IF 

     ELSE

        SELECT "OK"
        FROM   dis_cuenta
        WHERE  folio = reg.folio
        AND    subcuenta = 7
        GROUP BY 1
     
        IF STATUS <> NOTFOUND THEN
           LET bandera = 1
        END IF

     END IF

IF bandera = 0 THEN

   DECLARE cur_act CURSOR FOR 
   
         SELECT a.n_seguro      ,
                a.n_seguro_ent  ,
                a.rfc_ent       ,
                a.cve_ced_cuenta,
                a.nro_ctrl_icefa
         FROM   tra_det_trasp_sal a
         WHERE  a.folio         = reg.folio
   
   FOREACH cur_act INTO reg_llave.*
    
      UPDATE tra_mae_icefa
      SET    tra_mae_icefa.status      = 7
      WHERE  tra_mae_icefa.n_seguro    = reg_llave.n_seguro
      AND    tra_mae_icefa.nss         = reg_llave.n_seguro_ent
      AND    tra_mae_icefa.rfc         = reg_llave.rfc_ent
      AND    tra_mae_icefa.icefa_cod   = reg_llave.cve_ced_cuenta
      AND    tra_mae_icefa.nro_int_cta = reg_llave.nro_ctrl_icefa
   -- AND    tra_mae_icefa.status      = 8
   
   END FOREACH
 
END IF

END FUNCTION


FUNCTION separa_nss(sep_folio)
#tp----------------------------

DEFINE  xx               , 
        sep_folio  INTEGER

WHENEVER ERROR CONTINUE

DROP table v_viv 

   CREATE TEMP TABLE v_viv(n_seguro char(011),cont_servicio INTEGER)
   
WHENEVER ERROR STOP

INSERT into v_viv 
SELECT unique n_seguro,cont_servicio
FROM   tra_det_trasp_sal 
WHERE  folio = sep_folio 
AND    saldo_viv_92 > 0

INSERT into v_viv 
SELECT unique n_seguro,cont_servicio
FROM   tra_det_trasp_int
WHERE  folio = sep_folio 
AND    int_viv_92 > 0

END FUNCTION

FUNCTION f_marca_cuenta(vnss,vmarca_entra,vvvv_corr)
#fd------------------------------------------

DEFINE l_fecha_ini                DATE
DEFINE ejecuta                    CHAR(300),
       vvvv_corr                  INTEGER
       
DEFINE vnss                       CHAR(11),
       vmarca_entra                       ,
       vmarca_estado                      ,
       vcodigo_rechazo            SMALLINT,
       vusuario                   CHAR(008)
DEFINE xcodigo_marca                      ,
       xcodigo_rechazo            SMALLINT

LET    vusuario      =  NULL

SELECT USER
INTO   vusuario
FROM   tab_afore_local
GROUP BY 1

####################################

LET    l_fecha_ini    =   NULL

SELECT a.fecha_ini
INTO   l_fecha_ini
FROM   safre_af:cta_his_marca a
WHERE  a.nss              =       vnss 
AND    a.marca_cod        =       250
AND    a.correlativo      =       vvvv_corr
AND    a.fecha_fin        =       g_fecha_conversion
AND    a.estado_marca     =       0
AND    a.fecha_fin IS NOT NULL
GROUP BY 1

IF (l_fecha_ini IS NOT NULL AND
    l_fecha_ini <> '12/31/1899') THEN

EXECUTE sql_desmarca  USING vnss          ,
                            vmarca_entra  ,
                            vvvv_corr     ,
                            l_fecha_ini

END IF

END FUNCTION
