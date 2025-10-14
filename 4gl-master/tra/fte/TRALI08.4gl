##############################################################################
#Owner             => E.F.P.                                                  
#Programa TRALI08  => PANTALLA DE PROVISION DE ICEFA - ISSSTE
#Fecha creacion    => 04 DE MAYO DE 1999                                
#By                => JESUS DAVID YANEZ MORENO                                  
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 10 DE ENERO DEL 2005
#Sistema           => TRA-ICE-ISSSTE                                            
##############################################################################

DATABASE safre_af

GLOBALS
    	DEFINE  enter                 CHAR(001),            
                txt		      CHAR(300),
 		titulo		      CHAR(300)


	DEFINE                       
        	tipo         SMALLINT,   
                aux_verifica INTEGER,
        	vfecha       DATE,       
        	seguro       CHAR(11),   
		hora         CHAR(05),            
        	rep          SMALLINT,
        	status_aux   SMALLINT,
                resp_imp     CHAR(001),
		x            SMALLINT

	DEFINE
		reg_fol      RECORD
                folio        INTEGER
        END RECORD


	DEFINE g_param_tra    RECORD LIKE seg_modulo.*  
	
	DEFINE n SMALLINT
	DEFINE i SMALLINT
	DEFINE total INTEGER
	DEFINE cadena CHAR(20)
	
	DEFINE  g_lista               CHAR(100) , 
        	comando               CHAR(100)   


        DEFINE
                idx             INTEGER,
                sal_cnt         INTEGER,
                array_sz        SMALLINT,
                over_size       SMALLINT,
                montot          LIKE dis_provision.monto_en_pesos,
                totregs         INTEGER 
        DEFINE
                reg_total       RECORD
                subcuenta		LIKE dis_provision.subcuenta,
                desc_subcuenta          CHAR(003),
                tipo_movimiento		LIKE dis_provision.tipo_movimiento,
                desc_mov                CHAR(007),
                monto_en_pesos		LIKE dis_provision.monto_en_pesos,
                cuenta			INTEGER
        END RECORD                
        
END GLOBALS                                       

GLOBALS "GLOB_REPS.4gl"
                                                  
MAIN
#m ---------------------------
    OPTIONS INPUT WRAP,                                                      
    PROMPT LINE LAST  ,                                                      
    ACCEPT KEY CONTROL-I                                                     
    DEFER INTERRUPT                                                          

  --LET hoy                 =                    TODAY        
    LET hora                =                    TIME        

    CALL init()                                                              

OPEN WINDOW w_TRAL0081 AT 2,2
WITH 19 ROWS, 75 COLUMNS
ATTRIBUTE(BORDER, FORM LINE 4) 
OPEN FORM f_TRAL0081 from "TRAL0081"
DISPLAY FORM f_TRAL0081 
    DISPLAY "                               <Ctrl-c> Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " TRALI08    RESUMEN PROVISION DE TRASPASO ICEFA-AFORE ISSSTE                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)


    INPUT BY NAME reg_fol.* WITHOUT DEFAULTS
        AFTER FIELD folio
            IF reg_fol.folio IS NULL THEN
                ERROR"CAMPO NO PUEDE SER NULO"
                NEXT FIELD folio
            END IF
        ON KEY (INTERRUPT)
            EXIT PROGRAM

        ON KEY (ESC)
            IF reg_fol.folio IS NULL THEN
                ERROR"CAMPO NO PUEDE SER NULO"
                NEXT FIELD folio               
            END IF
            DISPLAY "PROCESANDO INFORMACION" AT 19,1 ATTRIBUTE(REVERSE)   

            CALL  consulta()
            IF i > 1 THEN
                CALL verif_imp()
            END IF

EXIT INPUT
END INPUT
CLOSE WINDOW w_TRAL0081
END MAIN

      
FUNCTION verif_imp()
#vi---------------

               LET g_folio                =          reg_fol.folio
    WHILE TRUE
      PROMPT "DESEA IMPRIMIR REPORTE S/N? " ATTRIBUTE (REVERSE) FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                RUN comando
                DISPLAY"PROCESANDO REPORTE..." AT 19,1 ATTRIBUTE(REVERSE) SLEEP 2
                DISPLAY " "
                CALL asigna_globales()
                CALL genera_reporte()
                DISPLAY"PROCESO CONCLUIDO RUTA DEL REPORTE GENERADO ==>"," ",g_seg_modulo.ruta_listados AT 19,1 ATTRIBUTE(REVERSE) SLEEP 5
                EXIT WHILE
            ELSE
                DISPLAY"PROCESO CANCELADO..." AT 19,1 ATTRIBUTE(REVERSE) SLEEP 3 
                EXIT WHILE
            END IF
        END IF
    END WHILE        
END FUNCTION



FUNCTION init()
#i ---------------

#### PARA VARIABLES DEL GLOB_REP Y GOB_REPS#############

    SELECT  USER
    INTO    g_usuario
    FROM    tab_afore_local

    SELECT  *
    INTO    g_seg_modulo.*
    FROM    seg_modulo
    WHERE   modulo_cod = 'tra'

    LET HOY = TODAY

 
#########################################################

END FUNCTION       


FUNCTION consulta()
#c---------------

        SELECT count(*) 
        INTO   aux_verifica 
        FROM   dis_provision 
        WHERE  folio=reg_fol.folio AND 
               subcuenta in (13,14)  AND 
               tipo_movimiento  in (1,4)

IF (aux_verifica = 0) THEN                                                                
  PROMPT "FOLIO INEXISTENTE <enter para continuar>" ATTRIBUTE (REVERSE) FOR CHAR enter                                                                         
RETURN                                                                         
END IF                                                                         

	DECLARE cur_1 CURSOR FOR 

	SELECT subcuenta,
               tipo_movimiento,
               sum(monto_en_pesos),
               count(*)
	FROM   dis_provision 
	WHERE  folio  =   reg_fol.folio AND
               subcuenta in (13,14)  AND
               tipo_movimiento in (1,4)
	GROUP BY 1,2
	ORDER BY 1,2
LET i=1
LET montot=0

	FOREACH cur_1 INTO  reg_total.subcuenta,    
                            reg_total.tipo_movimiento,
                            reg_total.monto_en_pesos,
                            reg_total.cuenta
		CASE reg_total.subcuenta 
       			WHEN 13 LET reg_total.desc_subcuenta='SAR' EXIT CASE
			WHEN 14 LET reg_total.desc_subcuenta='VIV' EXIT CASE
		END CASE

        	CASE reg_total.tipo_movimiento
			WHEN 1 LET reg_total.desc_mov ='APORTE'   EXIT CASE
               		WHEN 4 LET reg_total.desc_mov ='INTERES'  EXIT CASE
        	END CASE
        	LET montot = montot + reg_total.monto_en_pesos
        	LET totregs=totregs +  reg_total.cuenta
			DISPLAY reg_total.* TO scr_1[i].*
                LET i=i+1

        END FOREACH

                DISPLAY montot,totregs TO montot,totregs

END FUNCTION        

FUNCTION asigna_globales()
#ag-----------------------

    LET     g_tabname     = 'dis_provision b '
    LET     g_folio       = reg_fol.folio

   SELECT COUNT(*)
   INTO   g_total_cuentas
   FROM   tra_det_trasp_sal_issste a
   WHERE  a.folio       =        reg_fol.folio

    SELECT a.fecha_provision
    INTO g_fecha_accion
    FROM   tra_his_dep_icefa a
    WHERE a.folio = reg_fol.folio
    GROUP BY 1

    LET    g_fecha_parti  = g_fecha_accion

    LET    g_tipo_desc1    = 'TRASPASO ICEFA ISSSTE POR TIPO MOVIMIENTO'
    LET    g_tipo_desc2    = 'TRASPASO ICEFA ISSSTE POR SUBCUENTA'
    LET    g_nombre_programa = "TRALI08"
    LET    g_tip_rep       ='ISSSTE'

END FUNCTION

