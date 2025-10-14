#########################################################
#Proyecto          => AFORE ( MEXICO )                                        			#
#Propietario       => E.F.P.                                                  						#
#Programa INTR0100  => REVERSA GENERACIÓN CONSTANCIAS	#
#Autor             => FRANCISCA SCHELL ROSALES								#
#Fecha             => 1 DE ABRIL 2013                                    				    #
#Sistema           => INT                                                    					    #
#########################################################

DATABASE safre_af

GLOBALS

    DEFINE
        opcion       SMALLINT,
        in_nss       CHAR(11),
        v_fecha_mov_banxico       DATE,
        hoy          DATE,
        enter        CHAR(1), 
        numero_reg               DECIMAL(10,0), 
        vnss               CHAR(11),
        v_det, v_cza       CHAR(70), 
        ejecuta            CHAR(200),
        vtipo_traspaso     CHAR(25), 
         vtipo_solicitud    CHAR(15)  

END GLOBALS

MAIN
      
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT
	INITIALIZE v_det, v_cza, ejecuta TO NULL
    
   CALL STARTLOG ("INTR0100.log")
   CALL ERRORLOG ("Version: 1.0")

    CALL inicio()            #i
    CALL proceso_principal() #pp

END MAIN

FUNCTION inicio()
#i-------------
    LET hoy = today
END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW INTR01001 AT 4,4 WITH FORM "INTR01001" ATTRIBUTE(BORDER)
    DISPLAY "INTR0100         REVERSO DE GENERACIÓN DE CONSTANCIAS                     " AT 3,1 ATTRIBUTE(REVERSE)   
    DISPLAY " < CTRL-C > Salir " AT 1,26              
    DISPLAY hoy USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)

    INPUT BY NAME opcion, v_fecha_mov_banxico,in_nss

      AFTER FIELD opcion
         IF opcion IS NULL THEN
            ERROR "Campo opcion reverso NO puede ser NULO"
            NEXT FIELD opcion
         ELSE
           IF opcion  > 5 THEN                                    
                ERROR "REVERSAR: 0=> 30280 , 1=> 30201, 2=> 30202 , 3=> 30501 , 4=>30203 , 5=>30221"
                SLEEP 3
                NEXT FIELD opcion
            ELSE
           		NEXT FIELD v_fecha_mov_banxico
           END IF # mayor a 5
         END IF    # Nulll option
 
        
     AFTER FIELD v_fecha_mov_banxico                                       
       IF v_fecha_mov_banxico IS NULL
         OR v_fecha_mov_banxico = ' ' THEN
           ERROR "Teclear fecha de Liquidación"
           NEXT FIELD v_fecha_mov_banxico
       ELSE
         IF  opcion <> 2
     		 	AND opcion <> 4  THEN
           EXIT INPUT  
       	 ELSE  	
       	  ERROR "PUEDE INGRESAR NSS PARA ESTA OPCION"	
           NEXT FIELD in_nss
         END IF	
       END IF         
  
   		AFTER FIELD in_nss
                EXIT INPUT                        
           
        ON KEY ( INTERRUPT )
            EXIT PROGRAM

    END INPUT
      	 
    WHILE TRUE
        PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR enter
        IF enter MATCHES "[sSnN]" THEN 
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                ERROR "PROCESO CANCELADO" ATTRIBUTE(REVERSE) 
                SLEEP 2
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

--    ERROR "PROCESANDO INFORMACION " 

    CASE opcion 
        WHEN 0 CALL rev_30280()            
        WHEN 1 CALL rev_30201()  
        WHEN 2 CALL rev_30202() # Al parecer esta constancia no la tiene PST
        WHEN 3 CALL rev_30501() 
        WHEN 4 CALL rev_30203() 
        WHEN 5 CALL rev_30221() 
       END CASE

    PROMPT  "PROCESO FINALIZADO, [Enter] para salir " ATTRIBUTE(REVERSE) 
    FOR enter

    CLOSE WINDOW INTR01001	

END FUNCTION

FUNCTION rev_30280()
    
    SELECT arch_det, arch_cza 
    INTO  v_det, v_cza
    FROM  tab_layout v
    WHERE v.layout_cod = 457

	LET ejecuta = "rm ",v_cza," ",v_det
    RUN ejecuta
        
	LET numero_reg = 0;

	DECLARE cur_30280 CURSOR FOR
	SELECT int_ctr_carta.nss	  
	  FROM int_ctr_carta
          WHERE int_ctr_carta.fecha_registro = v_fecha_mov_banxico
          AND int_ctr_carta.docto_cod    = 30280
          AND int_ctr_carta.edo_genera   = 20
  
  FOREACH cur_30280 INTO vnss
		UPDATE int_ctr_carta
        SET int_ctr_carta.edo_genera = 10,
               int_ctr_carta.fecha_genera = '',
               int_ctr_carta.hora_genera  = '',
               int_ctr_carta.consecutivo  = ''
        WHERE int_ctr_carta.nss = vnss
          AND int_ctr_carta.fecha_registro = v_fecha_mov_banxico
          AND int_ctr_carta.docto_cod    = 30280
          AND int_ctr_carta.edo_genera   = 20
			
	    IF  SQLCA.SQLERRD[3] = 0  THEN
			ERROR "NO SE REALIZÓ REVERSO"
		ELSE
			LET numero_reg = numero_reg +1;
		END IF	
			DISPLAY "REGISTROS REVERSADOS 30280: ",numero_reg AT 17,1
	END FOREACH
			DISPLAY "REGISTROS REVERSADOS 30280: ",numero_reg AT 17,1
END FUNCTION #rev_30280()

FUNCTION rev_30201()

    SELECT arch_det, arch_cza 
        INTO  v_det, v_cza
        FROM  tab_layout v
        WHERE v.layout_cod = 406

		LET ejecuta = "rm ",v_cza," ",v_det
		RUN ejecuta

LET numero_reg = 0;


	DECLARE cur_30201 CURSOR FOR
	SELECT int_ctr_carta.nss	  
	  FROM int_ctr_carta
          WHERE int_ctr_carta.fecha_registro = v_fecha_mov_banxico
          AND int_ctr_carta.docto_cod    = 30201
          AND int_ctr_carta.edo_genera   = 20
  
  FOREACH cur_30201 INTO vnss
       UPDATE int_ctr_carta  
	     SET int_ctr_carta.edo_genera = 10,
                 int_ctr_carta.fecha_genera = '',
                 int_ctr_carta.hora_genera  = '' ,
                 int_ctr_carta.consecutivo  = ''
           WHERE int_ctr_carta.nss = vnss
             AND int_ctr_carta.fecha_registro = v_fecha_mov_banxico
             AND int_ctr_carta.docto_cod = 30201
             AND int_ctr_carta.edo_genera   = 20
			
	    IF  SQLCA.SQLERRD[3] = 0  THEN
			ERROR "NO SE REALIZÓ REVERSO"
		ELSE
			LET numero_reg = numero_reg +1;
		END IF	
			DISPLAY "REGISTROS REVERSADOS 30201: ",numero_reg AT 17,1
	END FOREACH
			DISPLAY "REGISTROS REVERSADOS 30201: ",numero_reg AT 17,1
END FUNCTION #rev_30201()

FUNCTION rev_30202()

	SELECT arch_det, arch_cza 
	INTO  v_det, v_cza
	FROM  tab_layout v
	WHERE v.layout_cod = 407
             
	LET ejecuta = "rm ",v_cza," ",v_det
    RUN ejecuta

	LET numero_reg = 0;

IF in_nss IS NOT NULL THEN
       UPDATE int_ctr_carta
	   SET int_ctr_carta.edo_genera = 10, 
               int_ctr_carta.fecha_genera = '',
               int_ctr_carta.hora_genera  = '' ,
               int_ctr_carta.consecutivo  = ''
         WHERE int_ctr_carta.nss = in_nss
           AND int_ctr_carta.fecha_registro = v_fecha_mov_banxico
           AND int_ctr_carta.docto_cod    = 30202
           AND int_ctr_carta.edo_genera   = 20
  
  LET numero_reg = numero_reg +1;

ELSE 
	DECLARE cur_30202 CURSOR FOR
	SELECT int_ctr_carta.nss	  
	  FROM int_ctr_carta
          WHERE int_ctr_carta.fecha_registro = v_fecha_mov_banxico
          AND int_ctr_carta.docto_cod    = 30202
          AND int_ctr_carta.edo_genera   = 20

  FOREACH cur_30202 INTO vnss
        UPDATE int_ctr_carta
	   SET int_ctr_carta.edo_genera = 10, 
               int_ctr_carta.fecha_genera = '',
               int_ctr_carta.hora_genera  = '' ,
               int_ctr_carta.consecutivo  = ''
         WHERE int_ctr_carta.nss = vnss
           AND int_ctr_carta.fecha_registro = v_fecha_mov_banxico
           AND int_ctr_carta.docto_cod    = 30202
           AND int_ctr_carta.edo_genera   = 20
			
	    IF  SQLCA.SQLERRD[3] = 0  THEN
			ERROR "NO SE REALIZÓ REVERSO"
		ELSE
			LET numero_reg = numero_reg +1;
		END IF	
			DISPLAY "REGISTROS REVERSADOS 30202: ",numero_reg AT 17,1
	END FOREACH
END IF   
			DISPLAY "REGISTROS REVERSADOS 30202: ",numero_reg AT 17,1

END FUNCTION #rev_30202

FUNCTION rev_30501()
    
    SELECT arch_det, arch_cza 
        INTO  v_det, v_cza
        FROM  tab_layout v
        WHERE v.layout_cod = 458

		LET ejecuta = "rm ",v_cza," ",v_det
		RUN ejecuta
		LET numero_reg = 0;

	DECLARE cur_30501 CURSOR FOR
	SELECT int_ctr_carta.nss	  
	  FROM int_ctr_carta
          WHERE int_ctr_carta.fecha_registro = v_fecha_mov_banxico
          AND int_ctr_carta.docto_cod    = 30501
          AND int_ctr_carta.edo_genera   = 20
          AND int_ctr_carta.tipo_solicitud = 8 
  
  FOREACH cur_30501 INTO vnss
        UPDATE int_ctr_carta  
	   SET int_ctr_carta.edo_genera     = 10,
               int_ctr_carta.fecha_genera   = '',
               int_ctr_carta.hora_genera    = '' ,
               int_ctr_carta.consecutivo    = ''
         WHERE int_ctr_carta.nss            = vnss
           AND int_ctr_carta.fecha_registro = v_fecha_mov_banxico
           AND int_ctr_carta.docto_cod      = 30501
           AND int_ctr_carta.edo_genera     = 20
           AND int_ctr_carta.tipo_solicitud = 8 
			
	    IF  SQLCA.SQLERRD[3] = 0  THEN
			ERROR "NO SE REALIZÓ REVERSO"
		ELSE
			LET numero_reg = numero_reg +1;
		END IF	
			DISPLAY "REGISTROS REVERSADOS 30501: ",numero_reg AT 17,1
	END FOREACH
			DISPLAY "REGISTROS REVERSADOS 30501: ",numero_reg AT 17,1
END FUNCTION#rev_30501

FUNCTION rev_30203()

    SELECT arch_det, arch_cza 
        INTO  v_det, v_cza
        FROM  tab_layout v
        WHERE v.layout_cod = 408
 
 	LET ejecuta = "rm ",v_cza," ",v_det
             RUN ejecuta
             
	LET numero_reg = 0;

IF in_nss IS NOT NULL THEN 
       UPDATE int_ctr_carta
	   SET int_ctr_carta.edo_genera = 10, 
               int_ctr_carta.fecha_genera = '',
               int_ctr_carta.hora_genera  = '' ,
               int_ctr_carta.consecutivo  = ''
         WHERE int_ctr_carta.nss = in_nss
           AND int_ctr_carta.fecha_registro = v_fecha_mov_banxico
           AND int_ctr_carta.docto_cod    = 30203
           AND int_ctr_carta.edo_genera   = 20
  
  LET numero_reg = numero_reg +1;

ELSE 
	DECLARE cur_30203 CURSOR FOR
	SELECT int_ctr_carta.nss	  
	  FROM int_ctr_carta
          WHERE int_ctr_carta.fecha_registro = v_fecha_mov_banxico
          AND int_ctr_carta.docto_cod    = 30203
          AND int_ctr_carta.edo_genera   = 20
 
  FOREACH cur_30203 INTO vnss
        UPDATE int_ctr_carta  
           SET int_ctr_carta.edo_genera = 10,     
               int_ctr_carta.fecha_genera = '',
               int_ctr_carta.hora_genera  = '' ,
               int_ctr_carta.consecutivo  = ''
         WHERE int_ctr_carta.nss = vnss
           AND int_ctr_carta.fecha_registro = v_fecha_mov_banxico
           AND int_ctr_carta.docto_cod = 30203
           AND int_ctr_carta.edo_genera   = 20
			
	    IF  SQLCA.SQLERRD[3] = 0  THEN
			ERROR "NO SE REALIZÓ REVERSO"
		ELSE
			LET numero_reg = numero_reg +1;
		END IF	
			DISPLAY "REGISTROS REVERSADOS 30203: ",numero_reg AT 17,1
	END FOREACH
	END IF 
			DISPLAY "REGISTROS REVERSADOS 30203: ",numero_reg AT 17,1
END FUNCTION#rev_30203()

FUNCTION rev_30221()

DEFINE selec_tab1               CHAR(2500)

    SELECT arch_det, arch_cza 
        INTO  v_det, v_cza
        FROM  tab_layout v
        WHERE v.layout_cod = 409

	LET ejecuta = "rm ",v_cza," ",v_det
    RUN ejecuta

	LET numero_reg = 0;

 	LET selec_tab1= " SELECT c.nss",
                            " FROM  taa_viv_recepcion a, afi_mae_afiliado b,",
                            "       int_ctr_carta c  ",
                            " WHERE a.nss           = b.n_seguro",
                            " AND   a.ident_operacion = '09'    ",
                            " AND   b.n_seguro = c.nss          ",
                            " AND  a.fecha_mov_banxico=c.fecha_registro ",
                            " AND  c.docto_cod = 30221          ",
                            " AND  c.edo_genera = 20            ",
                           " AND  b.tipo_solicitud IN(1,2,3,4,7,13,15,17) ",
                            " AND  b.fentcons IS NOT NULL       ",
                            " AND  a.tipo_traspaso  in(1,2,12,21,24,71,73) ",
                            " AND fecha_registro = '",v_fecha_mov_banxico,"'"   

   PREPARE apt_30221 FROM selec_tab1
   DECLARE cur_30221 CURSOR FOR apt_30221
  
  FOREACH cur_30221 INTO vnss
        UPDATE int_ctr_carta  
           SET int_ctr_carta.edo_genera = 10,     
                          int_ctr_carta.fecha_genera = '',
               int_ctr_carta.hora_genera  = '' ,
               int_ctr_carta.consecutivo  = ''
         WHERE int_ctr_carta.nss = vnss
           AND int_ctr_carta.fecha_registro = v_fecha_mov_banxico
           AND int_ctr_carta.docto_cod = 30221
           AND int_ctr_carta.edo_genera   = 20
			
	    IF  SQLCA.SQLERRD[3] = 0  THEN
			ERROR "NO SE REALIZÓ REVERSO"
		ELSE
			LET numero_reg = numero_reg +1;
		END IF	
			DISPLAY "REGISTROS REVERSADOS 30221: ",numero_reg AT 17,1
	END FOREACH
			DISPLAY "REGISTROS REVERSADOS 30221: ",numero_reg AT 17,1
END FUNCTION#rev_30221()