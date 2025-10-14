GLOBALS 

DEFINE ACCION CHAR(001)
DEFINE enter  CHAR(001)
DEFINE hoy    DATE

END GLOBALS 


FUNCTION despliega_nss(l_opc,l_tipo_solicitud,l_tipo_modificacion)
#dnss--------------------------------------------------------------
    DEFINE nss_nssasoc CHAR(011)
    DEFINE bnd_mod CHAR(01)
    DEFINE l_tipo_solicitud SMALLINT
    DEFINE l_tipo_modificacion SMALLINT
    DEFINE x_n_folio        DECIMAL(11,0)
    DEFINE l_opc  CHAR(001)
    DEFINE cont_1 INTEGER

    DEFINE arr_1 ARRAY[1000] OF RECORD
	   arc_1                 smallint, 
	   folio                 INTEGER,
	   diag_confronta        char(02),
	   clasifica_separacion  char(01),
	   clave_admon           CHAR(03),
	   nss                   CHAR(11),
	   nss_asociado          CHAR(11),
	   status_interno        SMALLINT,
	   estado_desc           CHAR(20)
    END RECORD

    DEFINE arr_2 ARRAY[1000] OF RECORD 
	   cve_cedente     CHAR(003)
    END RECORD

    DEFINE cuantos      SMALLINT
    DEFINE txt_nss      CHAR(1500)
    DEFINE txt_count     CHAR(1000)
    DEFINE cad_construct CHAR(50)
    DEFINE x_nss        CHAR(11)
    DEFINE arc_1        INTEGER
    DEFINE total_pa        INTEGER
    DEFINE tot_folio_ts        INTEGER
    DEFINE band         SMALLINT
    DEFINE band_c       SMALLINT

    OPEN WINDOW ventana_nss AT 2,2 WITH FORM "SEPC0101" ATTRIBUTE(BORDER)

    DISPLAY " [Esc] Busqueda                                              [ Ctrl-C ] Salir  " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " SEPM011      NSS PARA SOLICITAR MODIFICACION POR SEPARACION                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)


    CONSTRUCT BY NAME cad_construct ON b.folio, a.n_seguro, a.nss 
     ON KEY(ESC)
          LET int_flag = FALSE
          EXIT CONSTRUCT
    ON KEY(INTERRUPT)
       LET band_c = 0
       EXIT CONSTRUCT
   END CONSTRUCT

   IF NOT int_flag THEN 


 CASE l_opc 
WHEN "M"
          LET txt_nss = 
          " SELECT unique '',b.folio,b.diag_confronta  , ",    
                        " b.clasifica_separacion , ",
                        " b.clave_entidad_admon  , ", 
                        " b.n_seguro             , ", 
                        " c.nss_asociado         , ",
                        " ' '                    , ",
                        " ' '                    , ",
			               " c.clave_entidad_involucrado ",
          " FROM   sep_det_reg_sol_reclamante a , ",
                 " sep_det_solicitud          b , ",
                 " sep_det03_solicitud        c   ",
          " WHERE   ",cad_construct CLIPPED ,
            " AND    b.idsolicitudseparacion = a.correlativo ",
            " AND    b.idsolicitudseparacion = c.idsolicitudseparacion ",
            " AND    b.diag_confronta  <> '02' ",
            " AND    (b.resultado_operacion in ('',' ','01') OR  ",
            "         b.resultado_operacion is null) ",
            " AND    c.resultado_operacion <> '02' "
    EXIT CASE
OTHERWISE 

          LET txt_nss = 
          " SELECT unique '',b.folio,b.diag_confronta  , ",    
                        " b.clasifica_separacion , ",
                        " b.clave_entidad_admon  , ", 
                        " b.n_seguro             , ", 
                        " c.nss_asociado         , ",
                        " d.status_interno       , ",
                        " e.estado_desc          , ",
			               " c.clave_entidad_involucrado ",
          " FROM   sep_det_reg_sol_reclamante a , ",
                 " sep_det_solicitud          b , ",
                 " sep_det03_solicitud        c , ",
                 " afi_mae_modifica           d , ",
                 " afi_mae_afiliado           f , ",
                 " tab_status_afi             e , ",
                 " cta_his_marca              g   ",
          " WHERE   ",cad_construct CLIPPED ,
            " AND    b.idsolicitudseparacion = a.correlativo ",
            " AND    b.idsolicitudseparacion = c.idsolicitudseparacion ",
            " AND    b.diag_confronta  <> '02' ",
            " AND    (b.resultado_operacion in ('',' ','01') OR  ",
            "         b.resultado_operacion is null) ",
            " AND    c.resultado_operacion <> '02' ",
            " AND    d.n_seguro = f.n_seguro ",
            " AND    d.status_interno = f.status_interno "

        EXIT CASE

    END CASE


   IF l_tipo_modificacion = 5 THEN

    LET txt_nss = txt_nss CLIPPED,
                  " AND a.estado = 5 "
   ELSE 

    LET txt_nss = txt_nss CLIPPED,
                  " AND (a.estado >= 52 OR a.estado IN (10,11,12)) "

   END IF 


      CASE l_opc
      WHEN "M" 
       IF l_tipo_modificacion = 5 THEN
           LET txt_nss = txt_nss CLIPPED ,
           "AND   c.nss_asociado NOT IN (SELECT d.n_seguro FROM ",
           "                             afi_mae_modifica d     ",
           "                      WHERE d.status_interno   ",
           "                             IN (100,130,230,220,160,200))   ",
           "AND   b.n_seguro      NOT IN (SELECT d.n_seguro FROM ",
           "                             afi_mae_modifica d     ",
           "                      WHERE d.status_interno   ",
           "                             IN (100,130,230,220,160,200))   "
       ELSE 
           LET txt_nss = txt_nss CLIPPED ,
           "AND   b.n_seguro NOT IN (SELECT d.n_seguro FROM ",
           "                             afi_mae_modifica d     ",
           "                      WHERE d.status_interno   ",
           "                             IN (130,220,230,160))   ",
           "AND   c.nss_asociado  NOT IN (SELECT d.n_seguro FROM ",
           "                             afi_mae_modifica d     ",
           "                      WHERE d.status_interno   ",
           "                             IN (130,220,230,160))   "
       END IF
        EXIT CASE
      WHEN "C"
       IF l_tipo_modificacion = 5 THEN
           LET txt_nss = txt_nss CLIPPED ,
           "AND   c.nss_asociado   = d.n_seguro  ",
           "AND   d.status_interno = e.estado_cod ",
           "AND   c.nss_asociado = g.nss ",
           "AND   g.marca_cod = 620 "
       ELSE 
           LET txt_nss = txt_nss CLIPPED ,
           "AND   b.n_seguro = d.n_seguro  ",
           "AND   d.status_interno = e.estado_cod ",
           "AND   b.n_seguro = g.nss ",
           "AND   g.marca_cod = 620 "
       END IF
        EXIT CASE
      WHEN "D"
       IF l_tipo_modificacion = 5 THEN
           LET txt_nss = txt_nss CLIPPED ,
           "AND   b.n_seguro   = d.n_seguro  ",
           "AND   d.status_interno = e.estado_cod ",
           "AND   b.n_seguro = g.nss ",
           "AND   g.marca_cod = 620 "
       ELSE 
           LET txt_nss = txt_nss CLIPPED ,
           "AND   c.nss_asociado = d.n_seguro  ",
           "AND   d.status_interno = e.estado_cod ",
           "AND   c.nss_asociado = g.nss ",
           "AND   g.marca_cod = 620 "
       END IF
        EXIT CASE

      END CASE
           PREPARE qry_nss FROM txt_nss 
           DECLARE cur_nss CURSOR FOR qry_nss
WHILE TRUE
   
           LET cont_1 = 1
   
           FOREACH cur_nss INTO arr_1[cont_1].*,arr_2[cont_1].cve_cedente

             IF (l_tipo_modificacion = 5  AND 
                 l_opc               = "C"   )THEN 
                LET nss_nssasoc = arr_1[cont_1].nss_asociado
             END IF

             IF (l_tipo_modificacion = 5  AND 
                 l_opc               = "D"   )THEN 
                LET nss_nssasoc = arr_1[cont_1].nss
             END IF

             IF (l_tipo_modificacion = 9  AND 
                 l_opc               = "C"   )THEN 
                LET nss_nssasoc = arr_1[cont_1].nss
             END IF

             IF (l_tipo_modificacion = 9  AND 
                 l_opc               = "D"   )THEN 
                LET nss_nssasoc = arr_1[cont_1].nss_asociado
             END IF
              SELECT unique a.status_interno,b.estado_desc
	      INTO   arr_1[cont_1].status_interno,
	             arr_1[cont_1].estado_desc
	      FROM   afi_mae_modifica  a  ,
	             tab_status_afi    b  ,
                     afi_mae_afiliado  c
              WHERE  a.n_seguro       = nss_nssasoc
	      AND    a.status_interno = b.estado_cod
              AND    a.n_seguro       = c.n_seguro 
              AND    a.status_interno = c.status_interno

	      LET arr_1[cont_1].arc_1 = cont_1

              IF (arr_1[cont_1].estado_desc IS NULL OR 
                  arr_1[cont_1].estado_desc = " ")     THEN
		 LET arr_1[cont_1].estado_desc = "FALTA CAPTURA"
   	      END IF

              LET cont_1 = cont_1 + 1
           END FOREACH
  
      IF cont_1 = 1 THEN
              PROMPT " NO EXISTEN REGISTROS ...<ENTER> PARA SALIR "
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
            " AND    b.idsolicitudseparacion = a.correlativo ",
            " AND    b.idsolicitudseparacion = c.idsolicitudseparacion ",
            " AND    b.diag_confronta  <> '02' ",
            " AND    (b.resultado_operacion in ('',' ','01') OR  ",
            "         b.resultado_operacion is null) ",
            " AND    c.resultado_operacion <> '02' "

       IF l_tipo_modificacion = 5 THEN

         LET txt_count = txt_count CLIPPED, 
            " AND    a.estado = 5 "
       ELSE
         LET txt_count = txt_count CLIPPED, 
            " AND (a.estado >= 52 OR a.estado in (10,11,12)) "

       END IF 

      PREPARE qry_txt_count FROM txt_count
      EXECUTE qry_txt_count INTO tot_folio_ts

          LET total_pa = cont_1 - 1 
	  DISPLAY BY NAME total_pa
	  DISPLAY BY NAME tot_folio_ts

           CALL SET_COUNT(cont_1-1)
            
             LET arc_1 = 0

CASE l_opc
WHEN "M"
  IF l_tipo_modificacion = 5 THEN
    DISPLAY " [Ctrl-p] Captura nss   [Ctrl-i] Captura asociado            [ Ctrl-C ] Salir " AT 1,1 ATTRIBUTE(REVERSE)
  ELSE
    DISPLAY " [Ctrl-i] Captura nss   [Ctrl-p] Captura asociado            [ Ctrl-C ] Salir " AT 1,1 ATTRIBUTE(REVERSE)
  END IF
EXIT CASE

WHEN "C"
  IF l_tipo_modificacion = 5 THEN
    DISPLAY " [Ctrl-p] Consultar nss                                      [ Ctrl-C ] Salir " AT 1,1 ATTRIBUTE(REVERSE)
  ELSE
    DISPLAY " [Ctrl-i] Consultar asociado                                 [ Ctrl-C ] Salir " AT 1,1 ATTRIBUTE(REVERSE)
  END IF
EXIT CASE

WHEN "D"
  IF l_tipo_modificacion = 5 THEN
    DISPLAY " [Ctrl-i] Consultar nss                                      [ Ctrl-C ] Salir " AT 1,1 ATTRIBUTE(REVERSE)
  ELSE
    DISPLAY " [Ctrl-p] Consultar asociado                                 [ Ctrl-C ] Salir " AT 1,1 ATTRIBUTE(REVERSE)
  END IF
EXIT CASE

END CASE
             DISPLAY ARRAY arr_1 TO  scr_1.*


             ON KEY (CONTROL-I)
             IF l_opc = "D" THEN 
                LET band = 1
                ERROR"OPCION INVALIDA..."
                EXIT DISPLAY 
             END IF
                
		       LET arc_1 = ARR_CURR()

             IF l_tipo_modificacion = 5 THEN 
	   	       LET x_nss = arr_1[arc_1].nss_asociado
             ELSE 
	   	       LET x_nss = arr_1[arc_1].nss
             END IF

          SELECT a.tipo_solicitud,
                 a.n_folio
          INTO   l_tipo_solicitud,
                 x_n_folio
          FROM   afi_mae_afiliado a
          WHERE  a.n_seguro = x_nss

          CASE l_opc 
		    WHEN "M" 
             LET ACCION = "M"
                CALL Modifica(x_nss,l_tipo_solicitud,x_n_folio)
             LET band = 1      
             EXIT DISPLAY                                      
             EXIT CASE
          OTHERWISE 
             LET ACCION = "C"
             IF (arr_1[arc_1].status_interno IS NULL OR 
                 arr_1[arc_1].status_interno = " ")     THEN

                 ERROR"REGISTRO SIN INFORMACION ..."     

             ELSE
       	        CALL Consulta(x_nss)
             END IF
		       EXIT CASE
           END CASE


        ON KEY (CONTROL-P)
             IF l_opc = "C" THEN 
                LET band = 1
                ERROR"OPCION INVALIDA..."
                EXIT DISPLAY 
             END IF

		       LET arc_1 = ARR_CURR()

             IF l_tipo_modificacion = 5 THEN 
	   	       LET x_nss = arr_1[arc_1].nss
             ELSE 
	   	       LET x_nss = arr_1[arc_1].nss_asociado
             END IF

          SELECT a.tipo_solicitud,
                 a.n_folio
          INTO   l_tipo_solicitud,
                 x_n_folio
          FROM   afi_mae_afiliado a
          WHERE  a.n_seguro = x_nss

          CASE l_opc 
		    WHEN "M" 
             LET ACCION = "M"
                CALL Modifica(x_nss,l_tipo_solicitud,x_n_folio)
             LET band = 1      
             EXIT DISPLAY                                      
             EXIT CASE
          OTHERWISE 
             LET ACCION = "C"
             IF (arr_1[arc_1].status_interno IS NULL OR 
                 arr_1[arc_1].status_interno = " ")     THEN

                 ERROR"REGISTRO SIN INFORMACION ..."     

             ELSE
       	        CALL Consulta(x_nss)
             END IF
		       EXIT CASE
           END CASE


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
