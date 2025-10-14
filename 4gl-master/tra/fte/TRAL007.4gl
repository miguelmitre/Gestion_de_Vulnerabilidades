##############################################################################
#Owner             => E.F.P.
#Programa TRAL007  => RESUMEN DE LA RECEPCION DE TRASPASOS ICEFA-AFORE
#                     (SAR Y VIVIENDA 92)
#Fecha creacion    => 04 DE MAYO DE 1999
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 21 DE FEBRERO DEL 2005 
#Sistema           => TRA-ICE-IMSS   
##############################################################################
DATABASE safre_af

GLOBALS

	DEFINE #char                                  
		HOY                   DATE                
                                                  
    	DEFINE #char                                  
        	enter                 CHAR(001),            
                txt		      CHAR(300),
 		titulo		      CHAR(300)

	DEFINE desc_banco ARRAY[30] OF RECORD
                banco   CHAR(15)
                END RECORD


	DEFINE                       
        	tipo         SMALLINT,   
        	vfecha       DATE,       
        	seguro       CHAR(11),   
		hora         CHAR(05),            
        	rep          SMALLINT,
        	status_aux   SMALLINT,
                resp_imp     CHAR(001),
		x            SMALLINT

	DEFINE
		reg_fol      RECORD
                folio        INTEGER,
                opc          SMALLINT
        END RECORD


	DEFINE g_param_tra    RECORD LIKE seg_modulo.*  
	
	DEFINE n SMALLINT
	DEFINE i SMALLINT
	DEFINE total INTEGER
	DEFINE cadena CHAR(20)
	
	DEFINE  g_lista               CHAR(100) , 
        	comando               CHAR(100)   


        DEFINE reg_trasp ARRAY[200] OF RECORD
                cve_ced_cuenta  CHAR(003),
                saldo_sar_92    DECIMAL(15,2),
                saldo_viv_92    DECIMAL(15,2),
                cuenta          INTEGER
        END RECORD,
                idx             INTEGER,
                sal_cnt         INTEGER,
                array_sz        SMALLINT,
                over_size       SMALLINT,
                reg_total       RECORD
                totalsar92      LIKE tra_det_trasp_sal.saldo_sar_92,
                totalviv92      LIKE tra_det_trasp_sal.saldo_viv_92,
                cuenta          INTEGER
        END RECORD                
       
        DEFINE  g_glob            RECORD
                codigo_afore      LIKE safre_af:tab_afore_local.codigo_afore,
                razon_social      LIKE safre_af:tab_afore_local.razon_social
                                  END RECORD
        DEFINE g_nom_prog         CHAR(07)

END GLOBALS                                       
                                                  
MAIN
#m ---------------------------
    OPTIONS INPUT WRAP,                                                      
    PROMPT LINE LAST  ,                                                      
    ACCEPT KEY CONTROL-I                                                     
    DEFER INTERRUPT                                                          

    CALL init()                                                              

OPEN WINDOW w_TRAL0071 AT 2,2
WITH 19 ROWS, 75 COLUMNS
ATTRIBUTE(BORDER, FORM LINE 4) 
OPEN FORM f_tral0071 from "TRAL0071"
DISPLAY FORM f_tral0071 
    DISPLAY "                               <Ctrl-c> Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " TRAL007     RESUMEN RECEPCION DE TRASPASO ICEFA-AFORE IMSS                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)


    INPUT BY NAME reg_fol.* WITHOUT DEFAULTS
        AFTER FIELD folio
            IF reg_fol.folio IS NULL THEN
                ERROR"CAMPO NO PUEDE SER NULO"
                NEXT FIELD folio
            END IF

        NEXT FIELD opc
            IF reg_fol.opc IS NULL THEN
                ERROR"CAMPO NO PUEDE SER NULO"
                NEXT FIELD opc
            END IF
            IF reg_fol.opc <> 1 AND reg_fol.opc <> 2 THEN
                ERROR"OPCION INVALIDA"
                NEXT FIELD opc
            END IF
        ON KEY (INTERRUPT)
            EXIT PROGRAM

        ON KEY (ESC)
            IF reg_fol.folio IS NULL THEN
                ERROR"CAMPO NO PUEDE SER NULO"
                NEXT FIELD folio               
            END IF
            IF reg_fol.opc IS NULL THEN
                ERROR"CAMPO NO PUEDE SER NULO"
                NEXT FIELD opc
            END IF
            IF reg_fol.opc <> 1 AND reg_fol.opc <> 2 THEN
                ERROR"OPCION INVALIDA"
                NEXT FIELD opc
            END IF
            DISPLAY "PROCESANDO INFORMACION" AT 19,1 ATTRIBUTE(REVERSE)   
            CALL  consulta()
            CALL verif_imp()
            CLEAR FORM  
            NEXT FIELD folio
EXIT INPUT
END INPUT
END MAIN

FUNCTION verif_imp()
#vi---------------

    WHILE TRUE
      PROMPT "DESEA IMPRIMIR REPORTE S/N? " ATTRIBUTE (REVERSE) FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
		START REPORT rpt_1 TO g_lista
                IF reg_fol.opc = 1 THEN
                        LET titulo = "APORTACIONES SAR Y VIVIENDA 92 TRA-AFO-ICE IMSS"  
		ELSE
                        LET titulo = "INTERESES  SAR  Y  VIVIENDA 92 TRA-AFO-ICE IMSS" 
		END IF 
                FOR i= 1 TO (sal_cnt-1)
	OUTPUT TO REPORT rpt_1(reg_trasp[i].*,titulo,desc_banco[i].banco)
                LET comando = "lp ", g_lista  
		END FOR 
                FINISH REPORT rpt_1
                RUN comando
                EXIT WHILE
            ELSE
                DISPLAY"PROCESO CONCLUIDO" AT 19,1 ATTRIBUTE(REVERSE) SLEEP 2
                 EXIT WHILE
            END IF
        END IF
    END WHILE        
END FUNCTION



FUNCTION init()
#i ---------------
   LET hoy                        =                   TODAY
   LET hora                       =                   TIME

   SELECT *
   INTO   g_param_tra.*
   FROM   seg_modulo
   WHERE modulo_cod = "tra"


   LET HOY                        =                   TODAY
   
   SELECT codigo_afore,
          razon_social
   INTO g_glob.*
   FROM safre_af:tab_afore_local

   LET g_nom_prog                 =                   "TRAL007"

END FUNCTION       


FUNCTION consulta()
#c---------------

LET array_sz = 200
OPEN WINDOW w_popup AT 2,2
WITH 19 ROWS, 75 COLUMNS
ATTRIBUTE(BORDER, FORM LINE 4)


DISPLAY "                             <Ctrl-c> Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
DISPLAY " TRAL007   GENERA REPORTE DE SALDO TRASPASO ICEFA-AFORE IMSS                   " AT 3,1 ATTRIBUTE(REVERSE)
DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE) 
OPEN FORM f_tral0072 from "TRAL0072"     
DISPLAY FORM f_tral0072

DISPLAY "Mover cursor utilizando F3, F4, y flechas." AT 18,1

CASE reg_fol.opc
WHEN 2
	LET txt = "SELECT cve_ced_cuenta, sum(int_sar_92), sum(int_viv_92),",
        " count(*) from   tra_det_trasp_int ",
        "where  folio = ",reg_fol.folio CLIPPED,
        "group by cve_ced_cuenta order by cve_ced_cuenta" CLIPPED

	SELECT sum(int_sar_92) SAR  ,
       		sum(int_viv_92) VIV  ,
       		count(*)
	INTO   reg_total.*
	FROM   tra_det_trasp_int
	WHERE  folio = reg_fol.folio;  
	DISPLAY reg_total.* TO scr_2.*
        LET g_lista = g_param_tra.ruta_listados CLIPPED, "/",
                       "SALSARVIV92.",hoy USING "DDMMYY","-",
                       hora[1,2],hora[4,5]      
EXIT CASE 

WHEN 1
     LET txt = "SELECT cve_ced_cuenta, sum(saldo_sar_92), sum(saldo_viv_92),",
     " count(*) from   tra_det_trasp_sal ",
     "where  folio = ",reg_fol.folio CLIPPED,
     "group by cve_ced_cuenta order by cve_ced_cuenta" CLIPPED     

	SELECT 	sum(saldo_sar_92) SAR  ,
       		sum(saldo_viv_92) VIV  ,
       		count(*)
	INTO  	reg_total.*
	FROM  	tra_det_trasp_sal
	WHERE  	folio = reg_fol.folio;  

	DISPLAY reg_total.* TO scr_2.*
        LET g_lista = g_param_tra.ruta_listados CLIPPED, "/",
                       "APOSARVIV92.",hoy USING "DDMMYY","-",
                       hora[1,2],hora[4,5]      

EXIT CASE
END CASE

LET over_size = FALSE
LET sal_cnt = 1
	PREPARE cur1 FROM txt
	DECLARE cursor_1 CURSOR FOR cur1

	FOREACH cursor_1 INTO reg_trasp[sal_cnt].*

	SELECT    icefa_desc
	INTO      desc_banco[sal_cnt].banco  
	FROM  	  tab_icefa   
        WHERE     icefa_cod = reg_trasp[sal_cnt].cve_ced_cuenta

        LET sal_cnt = sal_cnt + 1
        	IF sal_cnt > array_sz THEN
               	 LET over_size = TRUE
               	 EXIT FOREACH                
        	END IF
        END FOREACH

IF (sal_cnt = 1) THEN
        PROMPT "FOLIO INEXISTENTE <enter para continuar>" ATTRIBUTE (REVERSE) FOR CHAR enter
        LET idx = 1
        LET reg_trasp[idx].cve_ced_cuenta = NULL
ELSE
        IF over_size THEN
                MESSAGE "OVERFLOW SOLO SE PUEDEN DESPLEGAR",
                array_sz USING "<<<<<<"
        END IF

        CALL SET_COUNT(sal_cnt-1)
        LET int_flag = FALSE
        DISPLAY ARRAY reg_trasp TO scr_1.*
        LET idx = ARR_CURR()
        IF int_flag THEN
                LET int_flag = FALSE
                #CALL msg("No manufacturer code selected.")
                #LET reg_trasp[idx].cve_ced_cuenta = NULL
        END IF
END IF
CLEAR WINDOW w_popup
CLOSE WINDOW w_popup
END FUNCTION        


REPORT rpt_1 (reg_trasp,titulo,desc_banco)
        
	DEFINE reg_trasp  RECORD
                cve_ced_cuenta  CHAR(003),
                saldo_sar_92    DECIMAL(15,2),
                saldo_viv_92    DECIMAL(15,2),
                cuenta          INTEGER
        END RECORD                 

	DEFINE
  		titulo   	CHAR(50)

	DEFINE desc_banco  RECORD
               banco      CHAR(15)   
        END RECORD     

FORMAT 
	PAGE HEADER
        PRINT                                                                 
        SKIP 2 LINES                                                         
        PRINT                                                                
            COLUMN 02,g_nom_prog  CLIPPED,
            COLUMN 50, TODAY USING "DD/MM/YYYY"                              
        PRINT                                                                 

        PRINT COLUMN 030 ,g_glob.codigo_afore USING "###","  ",g_glob.razon_social CLIPPED

	PRINT 
	PRINT

            COLUMN 10,titulo

	PRINT
 	PRINT
        PRINT 
                COLUMN 1,"------------------------------------------------------------------"
                PRINT
		COLUMN 1,"BANCO",
		COLUMN 31,"SAR 92",
                COLUMN 48,"VIV 92",
 		COLUMN 56,"REGISTROS"
	PRINT 
                COLUMN 1,"------------------------------------------------------------------"

	ON EVERY ROW
                PRINT 
                COLUMN 1,reg_trasp.cve_ced_cuenta,
                COLUMN 5,desc_banco.banco,
                COLUMN 18,reg_trasp.saldo_sar_92,
		COLUMN 33,reg_trasp.saldo_viv_92,
                COLUMN 53,reg_trasp.cuenta
                

ON LAST ROW 
SKIP 1 LINE 
PRINT  COLUMN 1,"------------------------------------------------------------------"
PRINT  COLUMN 5,"TOTAL" ,
       COLUMN 22,SUM(reg_trasp.saldo_sar_92) USING "############.##",
       COLUMN 39,SUM(reg_trasp.saldo_viv_92) USING "############.##",
       COLUMN 56,SUM(reg_trasp.cuenta)       USING "#########"        
PRINT  COLUMN 1,"------------------------------------------------------------------" 

END REPORT
         
