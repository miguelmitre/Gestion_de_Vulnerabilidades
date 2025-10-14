##############################################################################
#Owner             => E.F.P.
#Programa TRAM073  => CONSULTA DE ESTADOS (ISSSTE)          
#Fecha creacion    => 04 DE MAYO DE 1999
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 19 DE ENERO DEL 2005
#Ultima   Mod      => ENERO DEL 2006.
#Sistema           => TRA-ICE-ISSSTE
##############################################################################
DATABASE safre_af
GLOBALS

   DEFINE   ga_estados         ARRAY[100]  OF   RECORD
            estado              LIKE  tra_det_atm_issste.estado,
            estado_descripcion  LIKE  tra_aut_estado.estado_descripcion,
            total               INTEGER
                              END RECORD


    DEFINE #glo #char
        enter                 CHAR(1) ,
        HORA                  CHAR(8)
     DEFINE HOY             DATE
     DEFINE g_folio_interno    INTEGER
   DEFINE   i         SMALLINT

END GLOBALS  

MAIN

    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           , PROMPT LINE LAST     ,
        MESSAGE LINE LAST

     CALL inicio()
     CALL proceso_consulta() 

END MAIN

FUNCTION inicio()
   LET   HOY   =  TODAY
END FUNCTION
 
FUNCTION proceso_consulta()
#-------------------------


DEFINE f_b SMALLINT
DEFINE arr_tra_ctr_folio ARRAY[100] OF RECORD LIKE tra_ctr_folio.*

DEFINE reg_tra_ctr_folio RECORD LIKE tra_ctr_folio.*

DEFINE estado_cnt   integer
DEFINE arr_c                ,
       i             SMALLINT
DEFINE  where_clause      CHAR(250)


LET f_b = 1
    OPEN WINDOW tram0731 AT 3,2 WITH FORM "TRAM073" ATTRIBUTE(BORDER)
    DISPLAY" TRAM073    CONSULTA DE EDO DE INVITACIONES ICEFA-AFORE ISSSTE                 " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "<ESC>Consultar <CTRL-C>Salir	                                                  " AT 1,1 ATTRIBUTE(REVERSE)

    WHILE  TRUE
    CONSTRUCT BY NAME where_clause  ON  folio_interno
       ON  KEY(ESC)
           EXIT  CONSTRUCT
       ON  KEY(CONTROL-C)
           EXIT  WHILE
    END CONSTRUCT
       CALL   cuenta_registros(where_clause) RETURNING  estado_cnt
       IF     estado_cnt          >  0  THEN
              DISPLAY  estado_cnt    TO  g_total
              DISPLAY "PROCESANDO INFORMACION ...                " at 20,2 ATTRIBUTE(REVERSE)
              CALL   arma_consulta(where_clause)
       ELSE
              DISPLAY "  NO EXISTEN REGISTROS CON ESE CRITERIO   " at 19,2 ATTRIBUTE(REVERSE)
       END IF

    END WHILE
END FUNCTION
##################################################
FUNCTION cuenta_registros(where_clause)
   DEFINE  where_clause      CHAR(250),
           sql_texto         CHAR(300),
           estado_cnt        INTEGER

   LET     sql_texto = "SELECT COUNT(estado) FROM tra_det_atm_issste ",
                       "WHERE ", where_clause CLIPPED
   PREPARE  etiqueta_cnt_estado   FROM  sql_texto
   EXECUTE  etiqueta_cnt_estado   INTO  estado_cnt
   FREE     etiqueta_cnt_estado
   RETURN  estado_cnt
END FUNCTION

FUNCTION  arma_consulta(where_clause)
   DEFINE where_clause CHAR(250)
   DEFINE instr_select    CHAR(250)
   DEFINE  prepvar      CHAR(200)
   LET    i           =  1
   LET prepvar = "SELECT a.estado,b.estado_descripcion,COUNT(a.estado) ",
                 "FROM    tra_det_atm_issste a,tra_aut_estado b ",
                 "WHERE ", where_clause  CLIPPED," ",
                 "AND     a.estado = b.estado_cod " ,
                 "GROUP BY 1,2 "
   PREPARE  prep_sel_estados   FROM   prepvar
   DECLARE  decl_sel_estados   CURSOR  FOR  prep_sel_estados
   OPEN  decl_sel_estados
   FOREACH  decl_sel_estados  INTO  ga_estados[i].*
      LET  i                   = i  +  1
   END FOREACH
   LET   i    =  i - 1
   CALL     despliega_estados()
END FUNCTION
#################################################
FUNCTION  despliega_estados()

  DISPLAY "                                              " at 20,2
    CALL     SET_COUNT(i)
  DISPLAY  ARRAY  ga_estados    TO   sa_estados.*
END FUNCTION
