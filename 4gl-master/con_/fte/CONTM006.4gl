###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #    
#Owner             => E.F.P                                               #
#Programa          => Generacion Archivos Promotores-Contabilidad         #
#Fecha             => 22 Mayo 1998                                        #
#By                => Jose Manuel Vizcaino                                #
#Sistema           => Contabilidad.                                       #
########################################################################### 
DATABASE safre_af
   GLOBALS
        DEFINE vcadena        CHAR(30)
        DEFINE vdireccion     CHAR(32) 
        DEFINE vestado        CHAR(40) 
        DEFINE hora           DATE
        DEFINE hoy            DATE
        DEFINE vresp          CHAR(1)
        DEFINE vnombre        CHAR(25)
   END GLOBALS  

   MAIN 
       CALL principal()
   END MAIN 

FUNCTION principal()
#-------------------

    LET hora = TIME
    LET hoy  = TODAY
    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CONTM003"
    ATTRIBUTE (BORDER)
    DISPLAY " CONTM006          REPORTE PARA VALIDACION DE RECAUDACION       " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                           < Ctrl-C > Salir                                " AT 1,2 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"   dd-mm-yyyy" AT 3,62 ATTRIBUTE(REVERSE)

     OPTIONS
            MESSAGE LINE LAST,
            PROMPT LINE LAST

     PROMPT "Desea Generar Proceso Promotores  S/N  ?:" 
     ATTRIBUTE(REVERSE) FOR vresp ATTRIBUTE(REVERSE)

     IF vresp MATCHES"[sS]" THEN 
              CALL genera_historico()
              CALL bco_prom()
              CALL referencias_promotor()
     ELSE  
        ERROR "PROCESO CANCELADO ..." ATTRIBUTE (REVERSE) SLEEP 2
     END IF
 
END FUNCTION


FUNCTION genera_historico()
#--------------------------

     DEFINE gr_genera RECORD
            nro_sol   INTEGER,  
            paterno   CHAR(40),
            materno   CHAR(40), 
            nombres   CHAR(40) 
     END RECORD
     
     DEFINE vcadena   CHAR(30)

     WHENEVER ERROR STOP

     START REPORT genera_hist TO "/PROMO/altaprom.txt"

       DECLARE c_historico CURSOR FOR
         SELECT nro_solicitud, paterno, materno, nombres
         FROM hist_promo  
         WHERE etiqueta = "1"
       FOREACH c_historico INTO gr_genera.*
         OUTPUT TO REPORT genera_hist(gr_genera.*)    
       END FOREACH
  
  END FUNCTION

REPORT genera_hist(rpt)
#----------------------

     DEFINE vcadena   CHAR(30)

     DEFINE rpt RECORD
            nro_sol   INTEGER,  
            paterno   CHAR(9),
            materno   CHAR(9),
            nombres   CHAR(31)
     END RECORD

     OUTPUT 
         PAGE LENGTH 20
         LEFT MARGIN 0
         TOP MARGIN 0
     FORMAT

     PAGE HEADER
       SKIP 0 LINES       

     ON EVERY ROW

     LET vcadena = rpt.paterno CLIPPED," ",rpt.materno CLIPPED," ",rpt.nombres CLIPPED

       PRINT
       COLUMN 01, rpt.nro_sol  USING "&&&&&&&&&&",
       COLUMN 11, "                  ",
       COLUMN 30, "ACC",
       COLUMN 33, "9",
       COLUMN 34, rpt.paterno,
       COLUMN 43, "19970820",
       COLUMN 51, vcadena,
       COLUMN 81, "V",
       COLUMN 82, "A",
       COLUMN 137, rpt.nro_sol USING "&&&&&&&&&&",
       COLUMN 148, "1",
       COLUMN 149, "C"

       ON LAST ROW
          PAUSE "          PRESIONE  < ENTER > PARA CONTINUAR           "

    END REPORT

 ############################################################################
     FUNCTION bco_prom()
 ############################################################################
   
     DEFINE  vcadena        CHAR(35)
     DEFINE  vdireccion     CHAR(30)
     DEFINE  vestado        CHAR(30)
      
     DEFINE gr_promo RECORD
         nro_sol         INTEGER,  
         paterno         CHAR(9),
         materno         CHAR(9),
         nombres         CHAR(31),
         icefa_desc      CHAR(10),
         plaza           CHAR(5),
         cta_bancaria    CHAR(16)
     END RECORD

         START REPORT genera_banco TO "/PROMO/bco_prom.txt"
   
         DECLARE c_cur CURSOR FOR 
           SELECT a.nro_sol, a.paterno, a.materno, a.nombres, 
                  d.icefa_desc, e.plaza, e.cta_bancaria
           FROM  pro_mae_promotor a, tab_estado b, hist_promo c,
                 tab_icefa d, pro_cta_promotor e 
           WHERE a.estado  = b.estad_cod
           AND   e.codven  = a.codven
           AND   e.bco_cod = d.icefa_cod
           AND   e.codven  = c.codven
           AND   c.etiqueta = "1"
         
         FOREACH c_cur INTO gr_promo.*
                    OUTPUT TO REPORT genera_banco(gr_promo.*)
         END FOREACH     
END FUNCTION

REPORT genera_banco(rpt)
#-----------------------

     DEFINE rpt RECORD
         nro_sol         INTEGER,       
         paterno         CHAR(9),
         materno         CHAR(9),
         nombres         CHAR(31),
         icefa_desc      CHAR(10),
         plaza           CHAR(5),
         cta_bancaria    CHAR(16)
     END RECORD

     OUTPUT 
         PAGE LENGTH 2000
         LEFT MARGIN 0
         TOP MARGIN 0
     FORMAT

       PAGE HEADER
         SKIP 0 LINES       
 
     ON EVERY ROW
     LET vcadena = rpt.paterno CLIPPED," ",rpt.materno CLIPPED," ",rpt.nombres CLIPPED
               PRINT
                   COLUMN 001, rpt.nro_sol  USING "&&&&&&&&&&",
                   COLUMN 011, "                  ",
                   COLUMN 030, "BKA",
                   COLUMN 033, rpt.icefa_desc,
                   COLUMN 043, "19970820",
                   COLUMN 051, rpt.icefa_desc,
                   COLUMN 101, rpt.plaza,
                   COLUMN 116, vcadena,
                   COLUMN 152, rpt.cta_bancaria USING "&&&&&&&&&&&&&&&",
                   COLUMN 238, rpt.nro_sol USING "&&&&&&&&&&"

      ON LAST ROW
         PAUSE "      PRESIONE  <ENTER> PARA CONTINUAR      "

    END REPORT

FUNCTION referencias_promotor()
#------------------------------
 
     DEFINE  vdireccion     CHAR(30)
     DEFINE  vestado        CHAR(30)
     
     DEFINE gr_promo RECORD
         codven     DECIMAL(10,0),
         nro_sol    INTEGER,
         paterno    CHAR(9),
         materno    CHAR(9),
         nombres    CHAR(31),
         calle      CHAR(24),
         numero     CHAR(5),
         colonia    CHAR(30),
         estado     CHAR(25),
         codpos     CHAR(5) 
     END RECORD

     START REPORT genera_arc TO "/PROMO/ref_prom.txt"
     START REPORT reporte_referencia TO "/PROMO/reporte_ref.txt"   

     DECLARE reg_prom CURSOR FOR 
       SELECT a.codven,a.nro_sol, a.paterno, a.materno, a.nombres, calle, 
              numero, colonia, b.estad_desc, codpos
       FROM  pro_mae_promotor a, tab_estado b, hist_promo c
       WHERE a.estado = b.estad_cod
       AND   a.codven = c.codven
       AND   c.etiqueta = "1" 

       FOREACH reg_prom INTO gr_promo.*
               OUTPUT TO REPORT genera_arc(gr_promo.*)
               OUTPUT TO REPORT reporte_referencia(gr_promo.*)    
       END FOREACH     
     FINISH REPORT reporte_referencia
END FUNCTION

REPORT genera_arc(rpt)
#---------------------

     DEFINE rpt RECORD
         codven     DECIMAL(10,0),
         nro_sol    INTEGER,   
         paterno    CHAR(9),
         materno    CHAR(9),
         nombres    CHAR(31),
         calle      CHAR(24),
         numero     CHAR(5),
         colonia    CHAR(30),
         estado     CHAR(25),
         codpos     CHAR(5) 
     END RECORD
 
     OUTPUT 
         PAGE LENGTH 2000
         LEFT MARGIN 0
         TOP MARGIN 0
     FORMAT

       PAGE HEADER
         SKIP 0 LINES       
 
     ON EVERY ROW
     LET vcadena = rpt.paterno CLIPPED," ",rpt.materno CLIPPED," ",rpt.nombres CLIPPED
     LET vdireccion = rpt.calle CLIPPED, " ", rpt.numero CLIPPED 
     LET vestado = rpt.estado CLIPPED," ",rpt.codpos CLIPPED

     PRINT
     COLUMN 001, rpt.nro_sol  USING "&&&&&&&&&&",
     COLUMN 011, "                  ",
     COLUMN 030, "ADD",
     COLUMN 033, "9",
     COLUMN 034, rpt.paterno,
     COLUMN 043, "19970820",
     COLUMN 051, vcadena,
     COLUMN 086, vdireccion,
     COLUMN 121, rpt.colonia,
     COLUMN 156, vestado 

END REPORT

REPORT reporte_referencia(rpt)
#-----------------------------

  DEFINE rpt RECORD
         codven       DECIMAL(10,0),
         nro_sol      INTEGER,      
         paterno      CHAR(9),
         materno      CHAR(9),
         nombres      CHAR(31),
         calle        CHAR(24),
         numero       CHAR(5),
         colonia      CHAR(20),
         estad_desc   CHAR(25),
         cod_posp     CHAR(5) 
  END RECORD
	
     DEFINE vnombres                 CHAR(34)
     DEFINE l_estado		     CHAR(16)
     DEFINE vicefa_desc              LIKE tab_icefa.icefa_desc
     DEFINE vplaza                   LIKE pro_cta_promotor.plaza
     DEFINE vcta_bancaria            LIKE pro_cta_promotor.cta_bancaria

     DEFINE 
            aux_sexo			CHAR(10)   ,
            vrazon_social               CHAR(20)   ,
            vcont                       SMALLINT

    OUTPUT
        PAGE LENGTH 90
        TOP  MARGIN 0
        BOTTOM MARGIN 0
	LEFT MARGIN 0
	RIGHT MARGIN 0

    FORMAT
    
    PAGE HEADER
        
        SELECT razon_social INTO vrazon_social
            FROM tab_afore_local 

	PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"========================================"           ,
            COLUMN 040,"========================================"           ,
            COLUMN 080,"========================================"           ,
            COLUMN 120,"========================================"           ,
            COLUMN 160,"====="
        PRINT
            COLUMN 140,"FECHA   :",hoy USING "DD/MM/YY"    
        PRINT
            COLUMN 001, "CONTL005"
        PRINT
	    COLUMN 001, "    "
	PRINT
	    COLUMN 001, vrazon_social,
            COLUMN 035,"              REPORTE PARA VALIDACION DIRECCIONES PROMOTORES                  ",
            COLUMN 140,"NUM.PAG.:",pageno USING "####,###"
            SKIP 1 LINE
	PRINT
            COLUMN 001,"----------------------------------------"           ,
            COLUMN 040,"----------------------------------------"           ,
            COLUMN 080,"----------------------------------------"           ,
            COLUMN 120,"----------------------------------------"           ,
            COLUMN 160,"-----"
       SKIP 1 LINES
        PRINT 
	    COLUMN 005, "Codigo Promotor", 
	    COLUMN 027, "Nombre(s)",
            COLUMN 066, "Calle",
            COLUMN 095, "Numero",
            COLUMN 103, "Colonia",
            COLUMN 124, "Estado",
            COLUMN 144, "Codigo Postal "
        PRINT
            COLUMN 066, "Banco",
            COLUMN 095, "Plaza",
            COLUMN 124, "Cuenta Bancaria"
	PRINT
	    COLUMN 001,"========================================"           ,
            COLUMN 040,"========================================"           ,
            COLUMN 080,"========================================"           ,
            COLUMN 120,"========================================"           ,
            COLUMN 160,"====="

    ON EVERY ROW
       
    LET vcont = vcont + 1

    LET vnombres = rpt.paterno CLIPPED," ",rpt.materno CLIPPED," ",rpt.nombres CLIPPED

             SELECT d.icefa_desc, e.plaza, e.cta_bancaria
             INTO  vicefa_desc, vplaza, vcta_bancaria
             FROM  pro_mae_promotor a, tab_estado b, hist_promo c,
                   tab_icefa d, pro_cta_promotor e
             WHERE a.estado  = b.estad_cod
             AND   e.codven  = a.codven
             AND   e.bco_cod = d.icefa_cod
             AND   e.codven  = c.codven
             AND   a.codven  = rpt.codven
             AND   c.etiqueta = "1"
         
          PRINT 
	    COLUMN 005, rpt.nro_sol,
	    COLUMN 027, vnombres,
            COLUMN 066, rpt.calle,
            COLUMN 095, rpt.numero,
            COLUMN 103, rpt.colonia,
            COLUMN 127, rpt.estad_desc,
            COLUMN 137, rpt.cod_posp
          PRINT
            COLUMN 066, vicefa_desc,
            COLUMN 095, vplaza,
            COLUMN 127, vcta_bancaria
          PRINT
            COLUMN 066, "            "

       ON LAST ROW
      
       SKIP 1 LINES 
       PRINT COLUMN 60, "================"
       PRINT 
       COLUMN 60, "TOTAL MOVIMIENTOS DE PROMOTORES :", vcont USING "#####"
       SKIP 2 LINES 
       
       ERROR "PROCESO FINALIZADO" ATTRIBUTE (REVERSE) SLEEP 2
     
END REPORT
