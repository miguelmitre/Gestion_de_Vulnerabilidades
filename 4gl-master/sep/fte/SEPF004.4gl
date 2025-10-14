DATABASE safre_af 
GLOBALS 

DEFINE g_sql_1       CHAR(1000)
DEFINE pmarca_entra  SMALLINT   ,
       v_corr        INTEGER    ,
       pestado_marca SMALLINT   ,
       pmarca_causa  SMALLINT   ,
       pusuario      CHAR(010)

DEFINE g_folio INTEGER
DEFINE ACCION CHAR(001)
DEFINE enter  CHAR(001)
DEFINE HOY    DATE
DEFINE g_ejecuta CHAR(100)
DEFINE g_ejecuta_rep CHAR(100)

END GLOBALS 

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST  ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT


INITIALIZE g_folio TO NULL

LET HOY = TODAY

CALL init()

      OPEN WINDOW ventana_menu AT 2,2 WITH 20 ROWS, 78 COLUMNS ATTRIBUTE(BORDER)
      DISPLAY " SEPF004        NOTIFICACION DE REGISTROS OPERACION 28                         " AT  3,1 ATTRIBUTE(REVERSE)
      DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

       MENU "NOTIFICACION OP 28 "
       COMMAND KEY("N") "Notificacion" "Busqueda Registros por notificar op 28"
               CALL despliega_nss("C",1)
       COMMAND KEY("D") "Desmarca" "Desmarca de Parejas Notificadas"
               CALL despliega_nss("C",2)
       COMMAND "Salir" "Salir del Programa"
               EXIT PROGRAM
         END MENU
END MAIN

FUNCTION despliega_nss(l_opc,l_tipo_solicitud)
#dnss------------------------------------------

    DEFINE bnd_mod CHAR(01)
    DEFINE l_tipo_solicitud SMALLINT
    DEFINE x_n_folio        DECIMAL(11,0)
    DEFINE l_correlativo    INTEGER
    DEFINE l_opc  CHAR(001)
    DEFINE cont_1 INTEGER

    DEFINE arr_corr ARRAY[1000] OF RECORD 
           correlativo    INTEGER  
    END RECORD 

    DEFINE arr_1 ARRAY[1000] OF RECORD
	   arc_1                 smallint, 
	   folio                 INTEGER,
	   diag_confronta        char(02),
	   clasifica_separacion  char(01),
	   nss                   CHAR(11),
           edo_mar_invadido      CHAR(10),
	   nss_asociado          CHAR(11),
           edo_mar_asociado      CHAR(10),
	   estado                SMALLINT,
	   estado_des            CHAR(20)
    END RECORD

    DEFINE cuantos      SMALLINT
    DEFINE txt_nss      CHAR(1000)
    DEFINE txt_count     CHAR(1000)
    DEFINE cad_construct CHAR(50)
    DEFINE x_nss        CHAR(11)
    DEFINE arc_1        INTEGER
    DEFINE total_pa        INTEGER
    DEFINE tot_folio_ts        INTEGER
    DEFINE band         SMALLINT
    DEFINE band_c       SMALLINT

    OPEN WINDOW ventana_nss AT 2,2 WITH FORM "SEPF0041" ATTRIBUTE(BORDER)
if l_tipo_solicitud = 1 then 
    DISPLAY " [Crl-i]Dm invadido [Crl-o]Dm asociado [Crl-p]Genera lote op28  [Crl-c]Salir   " AT 1,1 ATTRIBUTE(REVERSE)
else
    DISPLAY " [Crl-i]Dm invadido [Crl-o]Dm asociado [Crl-c]Salir                            " AT 1,1 ATTRIBUTE(REVERSE)
end if

    DISPLAY " SEPF004       NSS EN ESPERA DE NOTIFICACION Y DESMARCA                              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)


    CONSTRUCT BY NAME cad_construct ON b.folio,a.n_seguro
     ON KEY(ESC)
          LET int_flag = FALSE
          EXIT CONSTRUCT
    ON KEY(INTERRUPT)
       LET band_c = 0
       EXIT CONSTRUCT
   END CONSTRUCT

   IF NOT int_flag THEN 


 CASE l_opc 
WHEN "C"

          LET txt_nss = 
          " SELECT unique '',b.folio,b.diag_confronta  , ",    
                        " b.clasifica_separacion , ",
                        " b.n_seguro             , ", 
                        " ' '                    , ",
                        " c.nss_asociado         , ",
                        " ' '                    , ",
                        " a.estado               , ",
                        " e.des_estado           , ",
                        " a.correlativo            ",
          " FROM   sep_det_reg_sol_reclamante a , ",
                 " sep_det_solicitud          b , ",
                 " sep_det03_solicitud        c , ",
                 " sep_estado_separacion      e  ",
          " WHERE   ",cad_construct CLIPPED ,
            " AND    a.correlativo = b.idSolicitudSeparacion ",
            " AND    b.idSolicitudSeparacion = c.idSolicitudSeparacion ",
            " AND    a.estado = e.estado " 

        EXIT CASE

    END CASE

    CASE l_tipo_solicitud 
    WHEN  1  
        LET txt_nss  = txt_nss clipped, " AND a.estado in (8,10) " 
      EXIT CASE
    WHEN 2
        LET txt_nss  = txt_nss clipped, " AND a.estado in (9,11) " 
      EXIT CASE
    END CASE

WHILE TRUE

      PREPARE qry_nss FROM txt_nss 
      DECLARE cur_nss CURSOR FOR qry_nss
   
           LET cont_1 = 1
   
           FOREACH cur_nss INTO arr_1[cont_1].*,arr_corr[cont_1].*

	      LET arr_1[cont_1].arc_1 = cont_1

              SELECT   "OK"
                FROM   cta_act_marca 
               WHERE   nss = arr_1[cont_1].nss
                 AND   marca_cod = 280
              GROUP BY 1

              IF STATUS = NOTFOUND THEN
                 LET arr_1[cont_1].edo_mar_invadido  = " "
              ELSE 
                 LET arr_1[cont_1].edo_mar_invadido  = "X"
              END IF

              SELECT   "OK"
                FROM   cta_act_marca 
               WHERE   nss = arr_1[cont_1].nss_asociado
                 AND   marca_cod = 280
              GROUP BY 1

              IF STATUS = NOTFOUND THEN
                 LET arr_1[cont_1].edo_mar_asociado  = " "
              ELSE 
                 LET arr_1[cont_1].edo_mar_asociado  = "X"
              END IF

              LET cont_1 = cont_1 + 1
           END FOREACH
  
      IF cont_1 = 1 THEN
           ERROR ""
           CLEAR FORM
           PROMPT " NO EXISTEN REGISTROS PARA NOTIFICAR...<ENTER> PARA SALIR "
           ATTRIBUTE(REVERSE)
           FOR CHAR enter
	        CLOSE WINDOW ventana_nss
           EXIT WHILE
	   ELSE  


          LET txt_count = 
          " SELECT COUNT(*) ",
          " FROM   sep_det_reg_sol_reclamante a , ",
                 " sep_det_solicitud          b , ",
                 " sep_det03_solicitud        c   ",
          " WHERE   ",cad_construct CLIPPED ,
            " AND    a.correlativo = b.idSolicitudSeparacion ",
            " AND    b.idSolicitudSeparacion = c.idSolicitudSeparacion "

      CASE l_tipo_solicitud 
      WHEN 1 
            LET txt_count = txt_count clipped, " AND    a.estado in (8,10) "
        EXIT CASE
      WHEN 2
            LET txt_count = txt_count clipped, " AND    a.estado in (9,11) "
      END CASE

      PREPARE qry_txt_count FROM txt_count
      EXECUTE qry_txt_count INTO tot_folio_ts

      LET total_pa = cont_1 - 1 
      DISPLAY BY NAME total_pa
      DISPLAY BY NAME tot_folio_ts

      CALL SET_COUNT(cont_1-1)
            
      LET arc_1 = 0

      DISPLAY ARRAY arr_1 TO  scr_1.*
 
      ON KEY (CONTROL-O)
	     LET arc_1 = ARR_CURR()
	     LET x_nss = arr_1[arc_1].nss_asociado
          CASE l_opc 
		WHEN "A" 
            LET ACCION = "A"
            LET band = 1
            EXIT DISPLAY
	         EXIT CASE

            WHEN "C" 
            LET ACCION = "C"

                 LET arc_1 = ARR_CURR()
                 LET g_folio = arr_1[arc_1].folio

             LET pmarca_entra  = 280 
             LET v_corr        = 0
             LET pestado_marca = 0
             LET pmarca_causa   = 280
             SELECT   correlativo
               INTO   l_correlativo
               FROM   cta_act_marca 
              WHERE   nss = arr_1[arc_1].nss_asociado
                AND   marca_cod = 280
             IF       l_correlativo        <>   arr_1[arc_1].folio   THEN
                      LET       arr_1[arc_1].folio     =  l_correlativo
             END   IF

             PREPARE qry_desmarca_asociado FROM g_sql_1

             EXECUTE qry_desmarca_asociado USING arr_1[arc_1].nss_asociado,
                                    pmarca_entra  ,
                                    arr_1[arc_1].folio  ,
                                    pestado_marca ,
                                    pmarca_causa  ,
                                    pusuario

             LET band = 1

             EXIT DISPLAY
  	     EXIT CASE
          END CASE

      ON KEY (CONTROL-I)
	     LET arc_1 = ARR_CURR()
	     LET x_nss = arr_1[arc_1].nss_asociado
          CASE l_opc 
		WHEN "A" 
            LET ACCION = "A"
            LET band = 1
            EXIT DISPLAY
	         EXIT CASE

            WHEN "C" 
            LET ACCION = "C"

                 LET arc_1 = ARR_CURR()
                 LET g_folio = arr_1[arc_1].folio

             LET pmarca_entra  = 280 
             LET v_corr        = 0
             LET pestado_marca = 0
             LET pmarca_causa   = 280
             SELECT   correlativo
               INTO   l_correlativo
               FROM   cta_act_marca 
              WHERE   nss = arr_1[arc_1].nss
                AND   marca_cod = 280
             IF       l_correlativo        <>   arr_1[arc_1].folio   THEN
                      LET       arr_1[arc_1].folio     =  l_correlativo
             END   IF

             PREPARE qry_desmarca FROM g_sql_1

             EXECUTE qry_desmarca USING arr_1[arc_1].nss,
                                    pmarca_entra  ,
                                    arr_1[arc_1].folio  ,
                                    pestado_marca ,
                                    pmarca_causa  ,
                                    pusuario

             LET band = 1

             EXIT DISPLAY
  	     EXIT CASE
          END CASE

    ON KEY (CONTROL-P)
         IF l_tipo_solicitud = 1 then 
                 LET g_ejecuta = 'fglgo SEPB106.4gi'
                 RUN g_ejecuta
                    LET band = 1
                    EXIT DISPLAY
         END IF
    ON KEY (INTERRUPT)
	        LET band = 0
                EXIT DISPLAY
    END DISPLAY 

          IF band = 0 THEN
              PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " 
              ATTRIBUTE(REVERSE)
              FOR CHAR enter
	      CLOSE WINDOW ventana_nss
              EXIT WHILE
          END IF
     END IF
END WHILE
      ELSE 
	      CLOSE WINDOW ventana_nss
      END IF

   END FUNCTION


FUNCTION init()
#i------------
 


LET g_sql_1 = 'EXECUTE PROCEDURE desmarca_cuenta(?,?,?,?,?,?) '
LET g_sql_1 = g_sql_1 CLIPPED


SELECT USER
INTO pusuario
FROM tab_afore_local
GROUP BY 1


END FUNCTION

