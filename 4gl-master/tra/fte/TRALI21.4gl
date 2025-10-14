##############################################################################
#Owner             => E.F.P.
#Programa TRALI21  => GENERA STATUS GENERAL DE ICEFAS        
#Fecha creacion    => 26 DE ABRIL DE 1999
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 02 DE FEBRERO DEL 2005
#                  => FEBRERO DEL 2006
#Sistema           => TRA-ICE-ISSSTE
##############################################################################
DATABASE safre_af
GLOBALS
    DEFINE #display array
        idx             INTEGER,                        
        sal_cnt         INTEGER,                        
        array_sz        SMALLINT,                       
        over_size       SMALLINT, 
        tama            INTEGER                       


    DEFINE #char                                  
        HOY                   DATE     ,        
        cuenta                INTEGER  
                                                  
    DEFINE #char                                  
        enter                 CHAR(01)            

    DEFINE                       
        tipo         SMALLINT,   
        vfecha       DATE,       
        seguro       CHAR(11),   
	hora         CHAR(05),            
        rep          SMALLINT,
        status_aux   SMALLINT,
        x            SMALLINT 
DEFINE v_estado SMALLINT
    DEFINE g_param_tra    RECORD LIKE seg_modulo.*  

	DEFINE 
        	n 	SMALLINT,
		i 	SMALLINT,
		total 	INTEGER,
		cadena 	CHAR(40)

	DEFINE  g_lista               CHAR(100) , 
        	comando               CHAR(100)   

END GLOBALS                                       
                                                  

MAIN



   DEFINE reg_cuenta  RECORD              
       status          LIKE    tra_mae_icefa_issste.status,   
		 des_estado      LIKE tra_status.des_estado,
       cuenta          INTEGER                         
   END RECORD                                              

	DEFINE reg_cuenta_aux ARRAY [30] OF RECORD
		status		LIKE 	tra_mae_icefa_issste.status,
		des_estado	LIKE 	tra_status.des_estado,
		cuenta		INTEGER
	END RECORD

OPTIONS INPUT WRAP,                                                      
PROMPT LINE LAST  ,                                                      
ACCEPT KEY CONTROL-I                                                     
DEFER INTERRUPT                                                          


LET HOY=TODAY        
LET hora=TIME        

    SELECT *                   
    INTO   g_param_tra.*       
    FROM   seg_modulo
    WHERE  modulo_cod = "tra"


OPEN WINDOW TRAC0301 AT 2,2 WITH FORM "TRAC0301"  ATTRIBUTE(BORDER)
DISPLAY "                             <Ctrl-c> Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)                                            

DISPLAY " TRALI21       CONSULTA GENERAL DE STATUS ICEFA-AFORE ISSSTE                   " AT 3,1 ATTRIBUTE(REVERSE)                                          
DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)                   

DISPLAY "PROCESANDO INFORMACION " AT 20,1 ATTRIBUTE(REVERSE)      


DECLARE cur_1 CURSOR FOR 
SELECT a.status,count(*) 
FROM   tra_mae_icefa_issste a    
WHERE  a.status in (1,21,30)
GROUP BY a.status 
ORDER BY a.status

LET cuenta = 1

FOREACH cur_1 INTO reg_cuenta.status,reg_cuenta.cuenta

      SELECT des_estado 
		INTO reg_cuenta.des_estado
		FROM tra_status 
		WHERE estado = reg_cuenta.status

      LET reg_cuenta_aux[cuenta].* = reg_cuenta.*
LET cuenta = cuenta + 1
END FOREACH

LET cadena='TOTAL CAPTURA PENDIENTE DE ENVIAR: '      

SELECT COUNT(*)
INTO total
from tra_mae_icefa_issste a
WHERE a.status in (1,21,30)

DISPLAY cadena ,total  TO cad,cuenta1  

DISPLAY "                                           " AT 20,1    
DISPLAY "Mover cursor utilizando F2, F3, y flechas." AT 20,1

        CALL SET_COUNT (cuenta - 1)                                
        LET int_flag = FALSE                                     
        DISPLAY ARRAY reg_cuenta_aux TO reg_cuenta.*                       
        LET idx = ARR_CURR()                                     
        IF int_flag THEN                                         
                LET int_flag = FALSE
        END IF                                                   


DISPLAY "                                           " AT 20,1
END MAIN
