##########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                       #
#Programa DISB039  => GENERA ARCHIVO VISUAL                              #
#Fecha             => 26 JUNIO DEL 2002  	                         #
#By                => MARCOS GODINEZ JIMENEZ                             #
#Sistema           => DIS 			                         #
##########################################################################
DATABASE safre_af
GLOBALS
    DEFINE hoy           DATE,
			  sw 				 SMALLINT

    DEFINE #glo #char
        enter                 CHAR(1) ,
        g_usuario             CHAR(8)

    DEFINE aux_pausa	   CHAR(1)
    DEFINE g_afore	   RECORD LIKE tab_afore_local.*
    DEFINE g_paramgrales   RECORD LIKE dis_parametro.*
    DEFINE g_lp_impresoras RECORD LIKE tab_cmd_impresora.*
    DEFINE G_LISTA  	   CHAR(100)
    DEFINE COMANDO  	   CHAR(100)
    DEFINE ejecuta  	   CHAR(100)
    DEFINE lp       	 CHAR(100)
    DEFINE vnom_archivo   CHAR(15)      
    DEFINE opc CHAR(01)
        
END GLOBALS

MAIN

    SELECT *
    INTO g_paramgrales.*
    FROM dis_parametro

    SELECT *
    INTO g_lp_impresoras.*
    FROM tab_cmd_impresora

    OPTIONS
    PROMPT LINE LAST

    LET hoy  = TODAY

    OPEN WINDOW disb029 AT 4,4 WITH FORM "DISB0391" ATTRIBUTE (BORDER)
    DISPLAY "                               < CTRL-C >                                      " AT 1,1  ATTRIBUTE(REVERSE)
    DISPLAY " DISB039                   GENERAR ARCHIVO VISUAL               " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING " dd-mm-yyyy" AT 3,65 ATTRIBUTE (REVERSE)

    MENU "VISUAL"
        COMMAND "Generar" "Genera archivo visual "
            CALL genera()     

        COMMAND "Salir" "Salir del Programa"
            EXIT PROGRAM
            CLOSE WINDOW disb039

    END MENU
 
    DISPLAY vnom_archivo TO vnom_archivo
    IF aux_pausa matches "[Ss]" THEN 
       PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR "
			  FOR CHAR enter
    ELSE
       PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
			  FOR CHAR enter
    END IF

    CLOSE WINDOW disb039

END MAIN


FUNCTION genera()

    LET INT_FLAG = FALSE

    INPUT BY NAME vnom_archivo ----WITHOUT DEFAULTS

        AFTER FIELD vnom_archivo                                   
            IF vnom_archivo IS NULL THEN                         
               ERROR "    EL NOMBRE DEL ARCHIVO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
               NEXT FIELD vnom_archivo                                
            END IF                            

            DATABASE safre_tmp
            WHENEVER ERROR CONTINUE
            DROP TABLE safre_tmp:tmp_nom_arch
            WHENEVER ERROR stop

            CREATE TABLE safre_tmp:tmp_nom_arch
            (
            n_registros                  CHAR(200)
            )                

            LET ejecuta = "cd ",g_paramgrales.ruta_rescate CLIPPED,"; ls > lista"

            RUN ejecuta

            LET comando = g_paramgrales.ruta_rescate CLIPPED,"/lista" CLIPPED

            LOAD FROM comando INSERT INTO safre_tmp:tmp_nom_arch

            SELECT "X" 
            FROM safre_tmp:tmp_nom_arch
            WHERE n_registros = vnom_archivo

            IF STATUS = NOTFOUND THEN
               ERROR  "NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"
	       SLEEP 2
	       LET int_flag = TRUE
               EXIT INPUT
            END IF
            WHENEVER ERROR STOP

        ON KEY(CONTROL-C)
	    LET int_flag = TRUE
            EXIT INPUT 

        ON KEY(INTERRUPT)
            PROMPT " PROCESO CANCELADO ... < ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM 

    END INPUT
    IF INT_FLAG THEN
       LET INT_FLAG = FALSE
       CLEAR FORM
       CLEAR SCREEN
       RETURN
    END IF

    WHILE TRUE

       PROMPT " ESTA SEGURO S/N ? " FOR CHAR aux_pausa
       IF aux_pausa MATCHES "[SsNn]" THEN
          IF aux_pausa MATCHES "[Ss]" THEN
             EXIT WHILE
          ELSE
             PROMPT " PROCESO CANCELADO ... < ENTER > PARA SALIR " 
						 FOR CHAR enter
             EXIT PROGRAM
	  END IF
       END IF

    END WHILE 

    ERROR " PROCESANDO INFORMACION " 

    DATABASE safre_af

    SELECT *,USER
    INTO   g_afore.*, g_usuario
    FROM   tab_afore_local


    LET G_LISTA = g_paramgrales.ruta_envio CLIPPED,"/",
		  vnom_archivo[1,6] CLIPPED,".visual"
							
    CALL genera_archivo() 

    ERROR "" 

    PROMPT " PROCESO TERMINADO ... < ENTER > PARA SALIR " FOR CHAR enter

END FUNCTION

FUNCTION genera_archivo()

    DEFINE nss_visual RECORD
        n_seguro             char(11)      
    END RECORD 

    DEFINE afiliado RECORD
        n_seguro             char(11)      ,
        n_unico              char(18)      ,
        n_rfc                char(13)      ,
        paterno              char(40)      ,
        materno              char(40)      ,
        nombres              char(40)      ,
        sexo                 char(1)       ,
        estadon              char(2)       ,
        nacionalidad         char(3)       ,  
        tip_prob             char(1)       ,
        fol_prob             char(10)      ,
        doc_prob             char(16)      
    END RECORD

    DEFINE patron RECORD
        reg_patronal         char(11)      ,
        reg_fed_contrib      char(13)
    END RECORD

    DEFINE afiliado_1 RECORD
        n_seguro             char(11)      ,
        n_unico              char(18)      ,
        n_rfc                char(13)      ,
        paterno              char(40)      ,
        materno              char(40)      ,
        nombres              char(40)      ,
        sexo                 char(1)       ,
        estadon              char(2)       ,
        nacionalidad         char(3)       ,  
        tip_prob             char(1)       ,
        fol_prob             char(10)      ,
        doc_prob             char(16)      , 
        reg_patronal         char(11)      ,
        reg_fed_contrib      char(13)
    END RECORD

    DEFINE
        ejecuta               CHAR(200),
        comando               CHAR(200)

   WHENEVER ERROR CONTINUE
   DATABASE safre_tmp
   DROP TABLE safre_tmp:nss_visual

   WHENEVER ERROR STOP

--   CREATE TEMP TABLE safre_tmp:nss_visual
   CREATE TABLE safre_tmp:nss_visual
          (n_seguro CHAR(11));

    LET ejecuta = "cd ",g_paramgrales.ruta_rescate CLIPPED, 
                    "; cut -c 13-23 ",vnom_archivo   CLIPPED, 
                    "> nss_visual"
    RUN ejecuta                                                              

    WHENEVER ERROR STOP

    LET comando = g_paramgrales.ruta_rescate CLIPPED,"/","nss_visual"

    LOAD FROM comando INSERT INTO safre_tmp:nss_visual
    DATABASE safre_af

    IF STATUS = NOTFOUND THEN
       DISPLAY  "NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"
       EXIT PROGRAM
    END IF                
 
    INITIALIZE afiliado.* TO NULL

    START REPORT listado TO G_LISTA     

      DECLARE cur_1 CURSOR FOR
	SELECT n_seguro 
        FROM   safre_tmp:nss_visual      
        GROUP BY n_seguro
 
        FOREACH cur_1 INTO nss_visual.n_seguro

           SELECT a.n_seguro, 
                  a.n_unico, 
                  a.n_rfc,  
                  a.paterno,
		  a.materno,
                  a.nombres, 
                  a.sexo,   
                  a.estadon, 
                  a.nacionalidad,  
                  a.tip_prob,     
                  a.fol_prob,    
                  a.doc_prob    
           INTO   afiliado.*
           FROM   safre_af:afi_mae_afiliado a  
           WHERE  a.n_seguro         = nss_visual.n_seguro

           IF SQLCA.SQLCODE <> 0 THEN
              CONTINUE FOREACH
           END IF

           SELECT c.reg_patronal, 
                  c.reg_fed_contrib              
           INTO   patron.*
           FROM   safre_af:afi_mae_patron c
           WHERE  c.n_seguro         = nss_visual.n_seguro
           GROUP  BY 1,2

           LET afiliado_1.n_seguro     = afiliado.n_seguro 
           LET afiliado_1.n_unico      = afiliado.n_unico 
           LET afiliado_1.n_rfc        = afiliado.n_rfc  
           LET afiliado_1.paterno      = afiliado.paterno
           LET afiliado_1.materno      = afiliado.materno
           LET afiliado_1.nombres      = afiliado.nombres 
           LET afiliado_1.sexo         = afiliado.sexo    
           LET afiliado_1.estadon      = afiliado.estadon 
           LET afiliado_1.nacionalidad = afiliado.nacionalidad   
           LET afiliado_1.tip_prob     = afiliado.tip_prob     
           LET afiliado_1.fol_prob     = afiliado.fol_prob    
           LET afiliado_1.doc_prob     = afiliado.doc_prob
           LET afiliado_1.tip_prob     = patron.reg_patronal     
           LET afiliado_1.fol_prob     = patron.reg_fed_contrib    

           OUTPUT TO REPORT listado(afiliado_1.*)

        END FOREACH
    FINISH REPORT listado
END FUNCTION
    

REPORT listado(afiliado)

    DEFINE afiliado RECORD
        n_seguro             char(11)      ,
        n_unico              char(18)      ,
        n_rfc                char(13)      ,
        paterno              char(40)      ,
        materno              char(40)      ,
        nombres              char(40)      ,
        sexo                 char(1)       ,
        estadon              char(2)       ,
        nacionalidad         char(3)       ,  
        tip_prob             char(1)       ,
        fol_prob             char(10)      ,
        doc_prob             char(16)      , 
        reg_patronal         char(11)      ,
        reg_fed_contrib      char(13)
    END RECORD

    OUTPUT
        PAGE   LENGTH 90
        LEFT   MARGIN 0
        RIGHT  MARGIN 150
        TOP    MARGIN 0
        BOTTOM MARGIN 0

    FORMAT

    ON EVERY ROW

         PRINT
             COLUMN 001,afiliado.n_seguro                           ,
             COLUMN 012,afiliado.n_unico                            ,
             COLUMN 030,afiliado.n_rfc                              ,
             COLUMN 043,afiliado.paterno                            ,
             COLUMN 083,afiliado.materno                            ,
             COLUMN 123,afiliado.nombres                            ,
             COLUMN 163,afiliado.sexo                               ,
             COLUMN 164,afiliado.estadon                            ,
             COLUMN 166,afiliado.nacionalidad                       ,  
             COLUMN 169,afiliado.tip_prob                           ,
             COLUMN 170,afiliado.fol_prob                           ,
             COLUMN 180,afiliado.doc_prob                           ,
             COLUMN 196,afiliado.reg_patronal                       ,
             COLUMN 207,afiliado.reg_fed_contrib    

END REPORT 


