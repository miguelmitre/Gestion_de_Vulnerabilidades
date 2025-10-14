###########################################################################
#MODULO      : SEP
#PROGRAMA    : SEPF017
#DESCRIPCION : LISTA PAREJAS DE SEPARACION EN ESPERA DE SER LIQUIDADAS   
#              Y QUE FUERON PREVIAMENTE PROVISIONADAS
#AUTOR       : JESUS YAÑEZ MORENO
#FECHA       : 3 abr 2010 
###########################################################################

DATABASE safre_af

GLOBALS 

  DEFINE g_folio               INTEGER

  DEFINE 
         g_enter               CHAR(001) ,
         g_ejecuta             CHAR(100) 

  DEFINE hoy                   DATE

  DEFINE cont_1                INTEGER

  DEFINE arr_1 ARRAY[400]      OF RECORD
         arc_1                 SMALLINT , 
         folio                 INTEGER  ,
         diag_confronta        CHAR(02) ,
         clasifica_separacion  CHAR(01) ,
         invadido              CHAR(11) ,
         asociado              CHAR(11) ,
         fecha_marca_infosar   DATE     ,
         traspaso_previo       CHAR(02) ,
         idSolicitudSeparacion INTEGER  
  END RECORD

  DEFINE txt_nss               CHAR(1000) , 
         txt_count             CHAR(1000) ,
         cad_construct         CHAR(0050) ,
         x_nss                 CHAR(0011)

  DEFINE arc_1                 ,
         total_pa              ,
         tot_folio_ts          INTEGER

  DEFINE band                  ,
         band_c                SMALLINT

END GLOBALS

MAIN

 OPTIONS INPUT WRAP,
 PROMPT LINE LAST  ,
 ACCEPT KEY CONTROL-I
 DEFER INTERRUPT

 CALL STARTLOG(FGL_GETENV("USER")||".SEPF017.log") 

 LET hoy = TODAY

    OPEN WINDOW ventana_nss AT 2,2 WITH FORM "SEPF017" ATTRIBUTE(BORDER)

    DISPLAY " [Esc] Liquidar    [Ctrl-C] Salir                                               " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " SEPF017    PAREJAS DE SEPARACION EN ESPERA DE SER LIQUIDADAS                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

          LET txt_nss = 
          " SELECT unique '',a.folio,b.diag_confronta  , ",    
                        " b.clasifica_separacion , ",
                        " a.n_seguro       , ", 
                        " a.nss            , ",
                        " b.fecha_marca_infosar , ",
                        " b.traspaso_previo , ",
                        " a.correlativo       ",
          " FROM   sep_det_reg_sol_reclamante a , ",
                 " sep_det_solicitud          b , ",
                -- " sep_det03_solicitud        c , ",
                 " sep_estado_separacion      e , ",
                 " afi_mae_afiliado           d   ",
          " WHERE    a.estado = 55 ", -- provisionada en espera de liquidarse
            " AND    a.correlativo = b.idSolicitudSeparacion ",
          --  " AND    a.correlativo = c.idSolicitudSeparacion ",
            " AND    b.clasifica_separacion in ('A','B','C') ",
            " AND    a.nss = d.n_seguro ",
            " AND    d.status_interno in (100,110,140,150,200) ",
            " ORDER BY a.correlativo "

         WHILE TRUE

           PREPARE qry_nss FROM txt_nss 
           DECLARE cur_nss CURSOR FOR qry_nss
   
           LET cont_1 = 1
   
           FOREACH cur_nss INTO arr_1[cont_1].*
             LET arr_1[cont_1].arc_1 = cont_1
                   LET cont_1              = cont_1 + 1
           END FOREACH
  
           IF cont_1 = 1 THEN
              ERROR ""
              CLEAR FORM
              PROMPT " Sin Registros para Liquidar...<enter> para Salir "
              ATTRIBUTE(REVERSE)
              FOR CHAR g_enter
              CLOSE WINDOW ventana_nss
              EXIT WHILE
           ELSE

              LET txt_count = 
              " SELECT COUNT(unique a.correlativo) ",
              " FROM   sep_det_reg_sol_reclamante a , ",
                     " sep_det_solicitud          b , ",
                   --  " sep_det03_solicitud        c , ",
                     " sep_estado_separacion      e , ",
                     " afi_mae_afiliado           d   ",
              " WHERE    a.estado = 55 ", -- provisionada en espera de liq
                " AND    a.correlativo = b.idSolicitudSeparacion ",
              --  " AND    a.correlativo = c.idSolicitudSeparacion ",
                " AND    b.clasifica_separacion in ('A','B','C') ",
                " AND    a.nss = d.n_seguro ",
                " AND    d.status_interno in (100,110,140,150,200) "

             PREPARE qry_txt_count FROM txt_count
             EXECUTE qry_txt_count INTO tot_folio_ts

             LET total_pa = cont_1 - 1 
             DISPLAY BY NAME total_pa
             DISPLAY BY NAME tot_folio_ts

             CALL SET_COUNT(cont_1-1)
            
             LET arc_1 = 0

             DISPLAY ARRAY arr_1 TO  scr_1.*

                ON KEY (ESC)
                
                   LET g_ejecuta = "fglgo SEPB017 "
                   RUN g_ejecuta
                   let band = 1
                   exit display
                ON KEY (CONTROL-I)
                   let band = 0
                
                ON KEY (INTERRUPT)
                   LET band = 0
                   EXIT DISPLAY

             END DISPLAY 

             IF band = 0 THEN
                PROMPT " Proceso Finalizado...<enter> para salir " 
                ATTRIBUTE(REVERSE)
                FOR CHAR g_enter
                CLOSE WINDOW ventana_nss
                EXIT WHILE
             END IF
           END IF
       END WHILE
   END MAIN
