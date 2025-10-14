##############################################################################
#Owner             => E.F.P.
#Programa TRACI30  => GENERA REPORTE STATUS DE ICEFAS               
#Fecha creacion    => 04 DE MAYO DE 1999
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 01 DE FEBRERO DEL 2005 
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
		cadena 	CHAR(20)

	DEFINE  g_lista               CHAR(100) , 
        	comando               CHAR(100)   
               
        DEFINE  g_glob            RECORD
                codigo_afore      LIKE safre_af:tab_afore_local.codigo_afore,
                razon_social      LIKE safre_af:tab_afore_local.razon_social
                                  END RECORD
        DEFINE g_nom_prog         CHAR(07)

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

    CALL init()

    SELECT *                   
    INTO   g_param_tra.*       
    FROM   seg_modulo
    WHERE  modulo_cod = "tra"


OPEN WINDOW TRAC0301 AT 2,2 WITH FORM "TRAC0301"  ATTRIBUTE(BORDER)
DISPLAY "                             <Ctrl-c> Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)                                            

DISPLAY " TRACI30         GENERA REPORTE DE STATUS ICEFA-AFORE ISSSTE                   " AT 3,1 ATTRIBUTE(REVERSE)                                          
DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)                   

DISPLAY "PROCESANDO INFORMACION " AT 20,1 ATTRIBUTE(REVERSE)      


DECLARE cur_1 CURSOR FOR 
SELECT a.status,count(*) 
FROM   tra_mae_icefa_issste a    
GROUP BY a.status 
ORDER BY a.status

    LET g_lista = g_param_tra.ruta_listados CLIPPED, "/",    
                  "STATUS_ICEFAS.",HOY USING "DDMMYY","-", 
                  hora[1,2],hora[4,5]                     

START REPORT rpt_1 TO g_lista
LET cuenta = 1

FOREACH cur_1 INTO reg_cuenta.status,reg_cuenta.cuenta

      SELECT des_estado 
		INTO reg_cuenta.des_estado
		FROM tra_status 
		WHERE estado = reg_cuenta.status

      LET reg_cuenta_aux[cuenta].* = reg_cuenta.*
LET cuenta = cuenta + 1
END FOREACH

LET cadena='TOTAL DE REGISTROS: '      

SELECT COUNT(*)
INTO total
from tra_mae_icefa_issste

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

FOR rep=1 TO cuenta-1
    IF rep                                   > 12 and         
                  reg_cuenta_aux[rep].status = 0      
    THEN                                           
                  CONTINUE FOR                                   
     ELSE                                       
                  OUTPUT TO REPORT rpt_1 (reg_cuenta_aux[rep].*)
    END IF
END FOR

FINISH REPORT rpt_1

   LET comando = "lp ", g_lista          
 
      
DISPLAY "                                           " AT 20,1
#DISPLAY cadena,total AT 22,40  
    WHILE TRUE                                                               
      PROMPT "DESEA IMPRIMIR REPORTE S/N? " ATTRIBUTE (REVERSE) FOR CHAR enter 
        IF enter MATCHES "[sSnN]" THEN                                       
            IF enter MATCHES "[sS]" THEN                                     
                RUN comando 
                DISPLAY "ARCHIVO GENERADO EN: ",g_lista CLIPPED AT 19,2 ATTRIBUTE(REVERSE)

                PROMPT " PROCESO FINALIZADO...<ENTER> PARA CONCLUIR .." FOR enter

		EXIT WHILE                                                   
            ELSE                                                             
                DISPLAY"PROCESO CONCLUIDO" AT 20,1 ATTRIBUTE(REVERSE) SLEEP 2
		EXIT PROGRAM                                                 
            END IF                                                           
        END IF                                                               
    END WHILE                                                                
END MAIN


REPORT rpt_1 (reg_cuenta)
                                                                
        DEFINE reg_cuenta RECORD                                
                status          LIKE    tra_mae_icefa_issste.status,        
                des_estado      LIKE    tra_status.des_estado,
                cuenta          INTEGER                         
        END RECORD                                              
                                                                
FORMAT 
	PAGE HEADER
        PRINT                                                                 
        SKIP 2 LINES                                                         
        PRINT                                                                
            COLUMN 02,g_nom_prog CLIPPED,
            COLUMN 50, TODAY USING "DD/MM/YYYY"
        PRINT
            COLUMN 50, TIME                                
        PRINT                                                                
 
        PRINT COLUMN 027,g_glob.codigo_afore USING "###","  ",g_glob.razon_social CLIPPED

	PRINT 
	PRINT
            COLUMN 10,  "    REPORTE DE STATUS DE ICEFA-AFORE ISSSTE        "

	PRINT
 	PRINT
        PRINT 
                COLUMN 5,"----------------------------------------------------"
                PRINT
		COLUMN 5,"STATUS",
		COLUMN 15,"DESCRIPCION",
		COLUMN 44,"NO. REGISTROS"
                PRINT 
                COLUMN 5,"----------------------------------------------------"

	ON EVERY ROW
                PRINT 
                COLUMN 5,reg_cuenta.status,
		COLUMN 15,reg_cuenta.des_estado,
		COLUMN 45,reg_cuenta.cuenta
                

ON LAST ROW 
SKIP 1 LINE 
PRINT  COLUMN 5,"----------------------------------------------------"
PRINT  COLUMN 15,"TOTAL DE REGISTROS" ,
       COLUMN 45,SUM(reg_cuenta.cuenta) USING "###########"
PRINT  COLUMN 5,"----------------------------------------------------" 

END REPORT
         
FUNCTION init()

   LET HOY                      =                  TODAY
   LET hora                     =                  TIME

   
    SELECT codigo_afore,
           razon_social
      INTO g_glob.*
      FROM safre_af:tab_afore_local

   LET  g_nom_prog              =                 "TRACI30"


END FUNCTION
