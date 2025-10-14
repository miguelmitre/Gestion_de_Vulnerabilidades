###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.       					      #
#Programa AFIL011  => GENERA PLANO PARA CONFRONTA(POSIBLES DUPLICADOS)        #
#Fecha             => 20 DE OCTUBRE DEL 2000                                  #
#Actualizado       => ALEJANDRO CAMPOS SUAREZ.                                #
#Sistema           => AFI                                                     #
#Actualizado       => EDUARDO RESENDIZ MEDINA   29 MARZO 2006                 #
###############################################################################
DATABASE safre_af
GLOBALS
        DEFINE enter char(001)
        DEFINE reg_2 RECORD #glo #reg_2
            folio            INTEGER ,  
            nss_sol          CHAR(11), 
            curp_sol         CHAR(18), 
            paterno          CHAR(40), 
            materno          CHAR(40), 
            nombres          CHAR(40), 
            reg_asociados    SMALLINT, 
            cve_ent_sol      CHAR(3) ,  
            cod_result       CHAR(2) ,
            fentcons         DATE    ,
            hora             CHAR(8) ,
            n_folio          DECIMAL(10,0),
            id_origen_reg    CHAR(01)
        END RECORD
   
        DEFINE reg_3 RECORD
            nss_sol          CHAR(11),
            nss_dup          CHAR(11),
            curp_dup         CHAR(18),
            cve_ent_dup      CHAR(03),
            paterno          CHAR(40),
            materno          CHAR(40),
            nombres          CHAR(40),
            diag_aclara      CHAR(2) ,
            fentcons         DATE    , 
            hora             CHAR(8) ,
            n_folio          DECIMAL(10,0),
            id_origen_reg    CHAR(01)
        END RECORD

        DEFINE #glo #integer
            cont              ,
            cont1             ,
            tot_registros     INTEGER

        DEFINE 
            HOY                 DATE       ,
	    G_LISTA	       	CHAR(500)  ,
	    G_PERM 	       	CHAR(500)  ,
	    cat            	CHAR(500)  ,
	    aux_pausa       	CHAR(1)    ,
	    char            	CHAR(1)    ,
	    vfolio2        	INTEGER    , 
	    vfolio         	INTEGER    

	DEFINE g_seg_modulo   	RECORD LIKE seg_modulo.*
        DEFINE cla_sel          CHAR(250)
        DEFINE vnss_sol         CHAR(11)
        DEFINE vcve_ent_sol     CHAR(03)
        DEFINE vcve_ent_dup     CHAR(03)
        DEFINE vcodigo_afore    CHAR(03)
        DEFINE vafore_desc_dup  CHAR(40)
        DEFINE vafore_desc_sol  CHAR(40)

END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    --DEFER INTERRUPT

    CALL STARTLOG("AFIL012.log")
    CALL inicio()  #i
    CALL proceso_principal()  #pp

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY = TODAY

    SELECT afore_cod
    INTO   vcodigo_afore
    FROM   tab_afore
    WHERE  marca = 1

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIL0111" ATTRIBUTE(BORDER)
    DISPLAY "AFIL012     GENERA ARCHIVO POSIBLES DUPLICADOS PARA CONFRONTA                  " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "          [ Esc ] Iniciar                              [ Ctrl-C ] Salir         " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio

      AFTER FIELD vfolio
       SELECT "X"
       FROM   afi_cza_not
       WHERE  folio = vfolio
       GROUP BY 1

      IF vfolio IS NULL THEN
        ERROR " NO EXISTE EL FOLIO "
        NEXT FIELD vfolio
      END IF

    ON KEY (ESC)
    ERROR " PROCESANDO INFORMACION " 

      CALL genera_reporte() #gr
    --  CALL pregunta()
    
      PROMPT " REPORTE GENERADO, [Enter] p/salir " FOR enter
      EXIT PROGRAM

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM

    END INPUT

    CLEAR WINDOW ventana_1
    CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION pregunta()
#p-----------------

    PROMPT " DESEA GENERAR EL ARCHIVO  S/N  ?  " FOR aux_pausa

END FUNCTION                                                         

FUNCTION genera_reporte()
#gr----------------------

    DEFINE  hora char(8)
    DEFINE gimpresion CHAR(300)

    LET hora = TIME

    SELECT * 
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    LET G_LISTA = g_seg_modulo.ruta_listados CLIPPED,"/" CLIPPED,
                  "POS_DUP_NOT.", HOY USING"DDMMYY"

    LET G_PERM  = "chmod 777 ", G_LISTA 
    RUN G_PERM

    START REPORT listado_2 TO G_LISTA

    DECLARE cur_6 CURSOR FOR

    SELECT a.nss_sol    ,
           a.cve_ent_sol,
           b.cve_ent_dup
    FROM   afi_det_not_sol a,
           afi_det_not_aso b
    WHERE  a.folio = vfolio
    AND    a.folio = b.folio
    AND    a.nss_sol = b.nss_sol

    FOREACH cur_6 INTO vnss_sol,vcve_ent_sol,vcve_ent_dup 
           
    LET cla_sel = "SELECT a.nss_sol    ,",
                         "a.nss_dup    ,",
                         "a.curp_dup   ,",
                         "a.cve_ent_dup,",
                         "a.paterno    ,",
                         "a.materno    ,",
                         "a.nombres    ,",
                         "a.diag_aclara,",
                         "''           ,",
                         "''           ,",
                         "''           ,",
                         "id_origen_reg ",
                  "FROM   afi_det_not_aso a ",
                  "WHERE  a.folio   = ? ",
                  "AND    a.nss_sol = ? "

    PREPARE claexe FROM cla_sel
    DECLARE cur_3 CURSOR FOR claexe

    DECLARE cur_2 CURSOR FOR
        SELECT a.folio,
               a.nss_sol,
               a.curp_sol,
               a.paterno,
               a.materno,
               a.nombres,
               a.reg_asociados,
               a.cve_ent_sol,
               a.cod_result, #agrego
               "", 
               "",
               id_origen_reg
        FROM   afi_det_not_sol a
        WHERE  a.folio   = vfolio
        AND    a.nss_sol = vnss_sol

    FOREACH cur_2 INTO reg_2.*
        IF reg_2.cve_ent_sol = vcodigo_afore THEN
            SELECT m.fentcons,
                   m.hora,
                   m.n_folio
            INTO   reg_2.fentcons,
                   reg_2.hora,
                   reg_2.n_folio
            FROM   afi_mae_afiliado m
            WHERE  m.n_seguro = reg_2.nss_sol
        ELSE
            LET reg_2.fentcons = "00/00/0000"
            LET reg_2.hora     = "00:00:00"
            LET reg_2.n_folio  = 0
        END IF

        SELECT afore_desc
        INTO   vafore_desc_sol
        FROM   tab_afore
        WHERE  afore_cod = reg_2.cve_ent_sol

        OUTPUT TO REPORT listado_2(reg_2.*,vafore_desc_sol) #l2
    END FOREACH

    END FOREACH

        FINISH REPORT listado_2

    LET gimpresion = "lp ",G_LISTA
    --LET gimpresion = "vi ",G_LISTA
    RUN gimpresion

END FUNCTION

REPORT listado_2(reg_2,rdesc_afore_sol)
#l2--------------------
        DEFINE reg_2 RECORD #glo #reg_2
            folio            INTEGER,  
            nss_sol          CHAR(11), 
            curp_sol         CHAR(18), 
            paterno          CHAR(40), 
            materno          CHAR(40), 
            nombres          CHAR(40), 
            reg_asociados    SMALLINT, 
            cve_ent_sol      CHAR(3) ,  
            cod_result       CHAR(2) ,
            fentcons         DATE    ,
            hora             CHAR(8) ,
            n_folio          DECIMAL(10,0),
            id_origen_reg    CHAR(01)
        END RECORD
   
        DEFINE reg_3 RECORD
            nss_sol          CHAR(11),
            nss_dup          CHAR(11),
            curp_dup         CHAR(18),
            cve_ent_dup      CHAR(03),
            paterno          CHAR(40),
            materno          CHAR(40),
            nombres          CHAR(40),
            diag_aclara      CHAR(2) ,
            fentcons         DATE    , 
            hora             CHAR(8) ,
            n_folio          DECIMAL(10,0),
            id_origen_reg    CHAR(01)
        END RECORD

        DEFINE des_origen_reg CHAR(15)
        DEFINE rdesc_afore_sol CHAR(40)
        DEFINE rdesc_afore_dup CHAR(40)

    OUTPUT
        TOP MARGIN 1
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 200
        PAGE LENGTH 90

    FORMAT
        PAGE HEADER
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s9H'
        PRINT '\033e\033(s218T\033(s18H\033(s12B'
        PRINT COLUMN 2,"AFIL012",
              COLUMN 70,"LISTADO POSIBLES DUPLICADOS PARA CONFRONTA",
              COLUMN 160, TODAY USING "dd/mm/yyyy"
        SKIP 1 LINE

        PRINT '\033e\033(s218T\033(s18H\033(s12'
        PRINT COLUMN 1,"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" 
        SKIP 1 LINE
        PRINT COLUMN 005,"NSS SOL.",
              COLUMN 029,"CURP SOL.",
              COLUMN 048,"A.PATERNO",
              COLUMN 061,"A.MATERNO",
              COLUMN 074,"NOMBRES",
              COLUMN 093,"NSS/",
              COLUMN 100,"CVE/DESC.ENT.SOL",
--              COLUMN 130,"FOLIO EN",
              COLUMN 125,"FOLIO",
              COLUMN 135,"ORIGEN REG."
        PRINT COLUMN 017,"NSS DUP.",
              COLUMN 029,"CURP DUP.",
              COLUMN 093,"ASOC",
              COLUMN 100,"CVE/DESC.ENT.DUP",
--              COLUMN 130,"afi_mea_afiliado",
              COLUMN 125,"CAPTURADO"
        PRINT COLUMN 1,"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" 
        SKIP 1 LINE

    
    ON EVERY ROW

    IF reg_2.id_origen_reg = '1' THEN
       LET des_origen_reg = "REGISTRO"
    END IF
    IF reg_2.id_origen_reg = '2' THEN
       LET des_origen_reg = "NO AFILIADO"
    END IF

      PRINT '\033e\033(s218T\033(s18H\033(s12'
      PRINT COLUMN 001,"ASO",
            COLUMN 005,reg_2.nss_sol,    
            COLUMN 029,reg_2.curp_sol,   
            COLUMN 048,reg_2.paterno CLIPPED," ",
            COLUMN 061,reg_2.materno CLIPPED," ",
            COLUMN 074,reg_2.nombres CLIPPED," ",
            COLUMN 093,reg_2.reg_asociados USING "--", 
            COLUMN 100,reg_2.cve_ent_sol," ",rdesc_afore_sol CLIPPED,
--            COLUMN 130,reg_2.n_folio USING"--------",   
            COLUMN 125,vfolio USING"------",
            COLUMN 135,reg_2.id_origen_reg CLIPPED, " ",des_origen_reg CLIPPED

    OPEN cur_3 
      USING reg_2.folio,reg_2.nss_sol
     WHILE TRUE
        FETCH cur_3 INTO reg_3.*
        IF STATUS = NOTFOUND THEN
           CLOSE cur_3
           EXIT WHILE
        ELSE
            IF reg_3.cve_ent_dup = vcodigo_afore THEN
                SELECT fentcons,
                       hora, 
                       n_folio
                INTO   reg_3.fentcons,
                       reg_3.hora,
                       reg_3.n_folio
                FROM   afi_mae_afiliado
                WHERE  n_seguro = reg_3.nss_dup
            ELSE
                LET   reg_3.fentcons = "00/00/0000" 
                LET   reg_3.fentcons = "00:00:00" 
                LET   reg_3.n_folio = 0
            END IF

            SELECT afore_desc
            INTO   vafore_desc_dup
            FROM   tab_afore
            WHERE  afore_cod = reg_3.cve_ent_dup

        END IF

      PRINT '\033e\033(s218T\033(s18H\033(s12'
      PRINT COLUMN 005,reg_3.nss_sol, 
            COLUMN 017,reg_3.nss_dup, 
            COLUMN 029,reg_3.curp_dup,
            COLUMN 048,reg_3.paterno CLIPPED," ",
            COLUMN 061,reg_3.materno CLIPPED," ",
            COLUMN 074,reg_3.nombres CLIPPED," ",
            COLUMN 100,reg_3.cve_ent_dup," ",vafore_desc_dup CLIPPED,
--            COLUMN 130,reg_3.n_folio USING"--------",
            COLUMN 125,vfolio USING"------",
            COLUMN 135,reg_3.id_origen_reg CLIPPED, " ",des_origen_reg CLIPPED
     END WHILE

      PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 90,"Pagina:",PAGENO USING "<<<<"
END REPORT
