#################################################################################
#MODULO      : SEP                                                              #
#PROGRAMA    : SEPF030                                                          #
#DESCRIPCION : LISTA PAREJAS DE SEPARACION EN ESPERA DE LIQUIDARSE Y MANDA      #
#            : EJECUTAR EL PROGRAMA DE VERIFICACION DE MOVIMIENTOS SEPB030      #
#AUTOR       : JESUS YAÑEZ MORENO                                               #
#FECHA       : 3 abr 2010                                                       #
#------------------------------MODIFICACIONES-----------------------------------#
#Requerimiento     => CPL-1751                                                  #
#Fecha y Autor     => 01-Oct-2014    Alejandro Chagoya Salazar                  #
#Descripcion       => Se agrega Status interno 140 al query pincipal            #
#################################################################################

DATABASE safre_af

GLOBALS 

  define g_folio               integer

  define 
         g_enter               char(001) ,
         g_ejecuta             char(100) 

  define hoy                   date

  define cont_1                integer

  define arr_1 ARRAY[400]      of record
   arc_1                 smallint , 
         diag_confronta        char(02) ,
         clasifica_separacion  char(01) ,
         invadido              CHAR(11) ,
         asociado              CHAR(11) ,
         fecha_marca_infosar   date     ,
         traspaso_previo       char(02) ,
   idSolicitudSeparacion integer  
  end record

  define txt_nss               CHAR(1000) , 
         txt_count             CHAR(1000) ,
         cad_construct         CHAR(0050) ,
         x_nss                 CHAR(0011)

  define arc_1                 ,
         total_pa              ,
         tot_folio_ts          INTEGER

  define band                  ,
         band_c                smallint

END GLOBALS

MAIN

 OPTIONS INPUT WRAP,
 PROMPT LINE LAST  ,
 ACCEPT KEY CONTROL-I
 DEFER INTERRUPT

 CALL STARTLOG(FGL_GETENV("USER")||".SEPF030.log")

  LET hoy = TODAY

    OPEN WINDOW ventana_nss AT 2,2 WITH FORM "SEPF030" ATTRIBUTE(BORDER)

    DISPLAY "  [Ctrl-I] Elegir [Ctrl-C] Salir                                               " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " SEPF030       PAREJAS DE SEPARACION EN ESPERA DE VERIFICACION                 " AT 3,1 ATTRIBUTE(REVERSE)
    
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    CONSTRUCT BY NAME cad_construct ON a.n_seguro ,
                                       a.nss      ,
                                       b.fecha_marca_infosar ,
                                       b.traspaso_previo  ,
                                       a.correlativo
     ON KEY(ESC)
         LET int_flag = FALSE
         EXIT CONSTRUCT
     ON KEY(INTERRUPT)
         LET band_c = 0
         EXIT CONSTRUCT
     END CONSTRUCT

   IF NOT int_flag THEN 

          LET txt_nss = 
          " SELECT unique '',b.diag_confronta  , ",    
                        " b.clasifica_separacion , ",
                        " a.n_seguro       , ", 
                        " a.nss            , ",
                        " b.fecha_marca_infosar , ",
                        " b.traspaso_previo , ",
                        " a.correlativo       ",
          " FROM   sep_det_reg_sol_reclamante a , ",
                 " sep_det_solicitud          b , ",
               --  " sep_det03_solicitud        c , ",
                 " sep_estado_separacion      e , ",
                 " afi_mae_afiliado           d   ",
          " WHERE   ",cad_construct CLIPPED ,
            " AND    a.correlativo = b.idSolicitudSeparacion ",
          --  " AND    a.correlativo = c.idSolicitudSeparacion ",
            " AND    b.clasifica_separacion in ('C') ",
            " AND    a.nss = d.n_seguro ",
            " AND    d.status_interno in (100,110,140,150,200) ",   #CPL-1751
            " AND    a.estado = 52 " ,-- en espera de verificacion
            " ORDER by a.correlativo "

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
              PROMPT " No Existen Registros para Verificar.<ENTER> para Salir "
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
                " WHERE    a.correlativo = b.idSolicitudSeparacion ",
              --  " AND    a.correlativo = c.idSolicitudSeparacion ",
                " AND    b.clasifica_separacion in ('C') ",
                " AND    a.nss = d.n_seguro ",
                " AND    d.status_interno in (100,110,140,150,200) ",   #CPL-1751
                " AND    a.estado = 52 " -- en espera de 
                                            -- ser separado

             PREPARE qry_txt_count FROM txt_count
             EXECUTE qry_txt_count INTO tot_folio_ts
             LET total_pa = cont_1 - 1 
             DISPLAY BY NAME total_pa
             DISPLAY BY NAME tot_folio_ts

             CALL SET_COUNT(cont_1-1)

             LET arc_1 = 0

             DISPLAY ARRAY arr_1 TO  scr_1.*

             ON KEY (CONTROL-I)

                LET arc_1 = ARR_CURR()
                LET x_nss = arr_1[arc_1].asociado
                LET g_folio = arr_1[arc_1].idSolicitudSeparacion

                       LET g_ejecuta = "fglgo SEPB030 ",
                      arr_1[arc_1].idSolicitudSeparacion," ",
                                        arr_1[arc_1].invadido       ," ",
                                        arr_1[arc_1].asociado       ," ",
                                        arr_1[arc_1].diag_confronta ," ",
                                        arr_1[arc_1].clasifica_separacion

                        RUN g_ejecuta
                        let band = 1
                        EXIT DISPLAY
             ON KEY (INTERRUPT)
                LET band = 0
                EXIT DISPLAY

             END DISPLAY 
             IF band = 0 THEN
                PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " 
                ATTRIBUTE(REVERSE)
                FOR CHAR g_enter
                CLOSE WINDOW ventana_nss
                EXIT WHILE
             END IF
           END IF
        END WHILE
      ELSE 
        CLOSE WINDOW ventana_nss
      END IF

   END MAIN
