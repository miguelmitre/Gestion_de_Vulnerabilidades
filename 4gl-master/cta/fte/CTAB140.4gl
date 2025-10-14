###############################################################################i
##Proyecto           => SYSTEMA DE AFORES ( MEXICO )                          ##
##Owner              => E.F.P.                                                ##
##Programa CTAB140   => Desmarcar cuenta por identificacion por edad - 255    ##
##Sistema            => SAFRE                                                 ##
##Fecha Creacion     => 30 Jun 2010                                           ##
##By                 => JGHM                                                  ##
##Fecha Modifica     =>                                                       ##
################################################################################
DATABASE safre_af

GLOBALS 
 DEFINE  gr_marca                   RECORD LIKE cta_act_marca.* 
 DEFINE  gs_encon                   SMALLINT         
 DEFINE  gc_nss                     CHAR(11)         
 DEFINE  gc_query                   CHAR(300)       
 DEFINE  gs_Cancela                 SMALLINT        
 DEFINE  gc_User                    CHAR(10)
 DEFINE  gd_Fecha                   DATE 
END GLOBALS 

MAIN 
 DEFER INTERRUPT
 OPTIONS
   INPUT WRAP           ,
   PROMPT LINE LAST     ,
   ACCEPT KEY CONTROL-I

 CALL STARTLOG("CTAB140.log")
 CALL f_inicial() 
 CALL f_Captura()
END MAIN


### Datos iniciales
FUNCTION f_inicial()

 LET        gc_query   =   "  SELECT  *                              ",
                           "    FROM  cta_act_marca                  ",
                           "   WHERE  nss            =  ?            "
 PREPARE    p_BusMar            FROM  gc_query
 DECLARE    d_BusMar          CURSOR  FOR  p_BusMar

 LET        gc_query   =   ' EXECUTE  PROCEDURE desmarca_cuenta(?,?,?,?,?,?) '
 PREPARE    p_DesCue            FROM  gc_query 
 
 LET        gc_query   =   '  SELECT  USER, MAX(fecha_corte)         ',
                           '    FROM  cta_transf_edad                '
 PREPARE    p_SelFec            FROM  gc_query 

 LET        gc_query   =   '  UPDATE  cta_transf_edad                ',
                           '     SET  cod_rechazo     =   ?          ',
                           '   WHERE  nss             =   ?          ',
                           '     AND  fecha_corte     =   ?          ' 
 PREPARE    p_ActCta            FROM  gc_query
END FUNCTION


### Captura nss para desmarcar 
FUNCTION f_Captura()
 DEFINE    ls_VerMarca                SMALLINT
 DEFINE    lc_Mensaje                 CHAR(100) 
 DEFINE    lc_Resp                    CHAR(001) 
 DEFINE    ls_marca_entra             SMALLINT  
 DEFINE    li_correlativo             INTEGER
 DEFINE    ls_estado_marca            SMALLINT 
 DEFINE    ls_marca_causa             SMALLINT 
 DEFINE    ls_cod_rechazo             SMALLINT
 DEFINE    lc_hoy                     DATE 

 LET      gs_encon    =   0
 LET      gs_Cancela  =   0
 LET      lc_hoy      =   TODAY
 OPEN     WINDOW w01 AT 2,2 WITH FORM "CTAB1401"                                                    ATTRIBUTE(BORDER)
 DISPLAY "[Esc] Eliminar [Ctrl-C] Cancelar "                                              AT 01, 01 ATTRIBUTE(REVERSE) 
 DISPLAY lc_hoy  USING "DD-MM-YYYY"                                                       AT 01, 67 ATTRIBUTE(REVERSE)
 DISPLAY " ELIMNAR MARCA GENERADA POR EL PROCESO DE IDENTIFICACION POR EDAD             " AT 02, 01 ATTRIBUTE(REVERSE)
 DISPLAY "_____________________________________________________________________________ " AT 03, 01 ATTRIBUTE(NORMAL)

 WHILE TRUE 
      INPUT BY NAME     gc_nss 
                        WITHOUT DEFAULTS
     
         BEFORE  FIELD  gc_nss
                 LET    gc_nss = '' 
     
         ON KEY (CONTROL-C,  INTERRUPT)
                 CALL   f_DespMen("Proceso Terminado ...... ") 
                 LET    gs_Cancela = 1   
                 EXIT INPUT
     
         AFTER   FIELD  gc_nss
                 IF     gc_nss      IS NULL 
                  OR    gc_nss      =  ' '    THEN 
                        CALL  f_DespMen('NSS No puede ser blanco')
                        NEXT FIELD gc_nss
                 ELSE
                        EXIT INPUT 
                 END IF 
      END INPUT
     
      IF   gs_Cancela                       =   0  THEN
           PROMPT " ESTA SEGURO, QUE DESEA DESMARCAR ESTA CUENTA Si/No " FOR CHAR lc_Resp
           IF     lc_Resp    = 's' 
            OR    lc_Resp    = 'S'  THEN 
                LET   ls_VerMarca                =   0 
                CALL  f_VerMarca(gc_nss)             RETURNING ls_VerMarca, lc_Mensaje     
                IF    ls_VerMarca                =   1  THEN 
                      CALL  f_DespMen(lc_Mensaje)  
                ELSE
                      CALL  f_DespMen('Espere un momento, eliminando marca   ... ')
                      LET   ls_marca_entra       =   255
                      LET   li_correlativo       =   0
                      LET   ls_estado_marca      =   0
                      LET   ls_marca_causa       =   0
                      LET   ls_cod_rechazo       =   10
                      EXECUTE  p_SelFec            INTO  gc_User, 
                                                         gd_Fecha
                      EXECUTE  p_DesCue           USING  gc_nss,
                                                         ls_marca_entra,
                                                         li_correlativo,
                                                         ls_estado_marca,
                                                         ls_marca_causa,
                                                         gc_User
                      EXECUTE  p_ActCta           USING  ls_cod_rechazo,
                                                         gc_nss,
                                                         gd_Fecha
                      CALL  f_DespMen('Cuenta desmarcada ... ')
                END IF
           END IF
      ELSE 
           EXIT WHILE  
      END IF  
 END WHILE
END FUNCTION 


### Ver marcaje 
FUNCTION f_VerMarca(lc_nss)
 DEFINE      ls_VerMarca                   SMALLINT 
 DEFINE      lc_Mensaje                    CHAR(100)
 DEFINE      lc_nss                        CHAR(11)
 DEFINE      ls_encon                      SMALLINT 
 DEFINE      ls_entre                      SMALLINT 

 LET         ls_VerMarca           =       0
 LET         ls_encon              =       0
 LET         ls_entre              =       0
 INITIALIZE  gr_marca              TO      NULL
 
 FOREACH     d_BusMar              USING   lc_nss
                                   INTO    gr_marca.*
      LET    ls_entre              =       1
      IF     gr_marca.marca_cod    =       255   THEN 
             LET    ls_encon       =       1
             EXIT   FOREACH 
      END IF
 END FOREACH

 IF   ls_entre             =  0          THEN 
      LET    ls_VerMarca   =  1
      LET    lc_Mensaje    =  'NSS no existe en marcas '
 ELSE 
      IF     ls_encon      =       0     THEN 
           LET    ls_VerMarca   =  1
           LET    lc_Mensaje    =  'La cuenta no tiene la Marca Ident por Edad '
      ELSE
           LET    ls_VerMarca   =  0
           LET    lc_Mensaje    =  ''
      END IF
 END IF

 RETURN      ls_VerMarca, 
             lc_Mensaje
END FUNCTION 


### Despliega mensaje de error en pantalla, espera y limpia 
FUNCTION  f_DespMen(lc_Mens)
 DEFINE      lc_Mens                  CHAR(100)
 
 ERROR    lc_Mens 
 SLEEP    3
 ERROR    '' 
END FUNCTION 
