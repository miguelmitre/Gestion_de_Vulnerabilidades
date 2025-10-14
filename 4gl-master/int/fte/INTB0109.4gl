#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => INTB0109 Y INTB01007                          #
#Descripcion       => NOTIFICACION DE CONSTANCIA DE TRASPASO Y RE-- #
#                  => GISTRO.                                       #
#Sistema           => INT .                                         #
#Fecha             => 17 de abril del 2002   .                      #
#Por               => Laura Eugenia Cortes Guzman                   #
#Fecha Modif.      => 25 de Septiembre del 2002.                    #
#Por               => Laura Eugenia Cortes Guzman                   #
#Req:1042          => JCPV 23/10/2012                               #
#CPL-1461          => FSR 12/11/2013 Se coloca folio como filtro    #
#CPL-1686          => FSR 18/08/2014 archivo paso por usuario       #
#                                                                   #
#CPL-3184             Se agregan los tipo traspaso 77               #
#                     Mauro Muñiz Caballero                         #
#                     9 de agosto de 2020                           #
#*******************************************************************#
DATABASE safre_af
GLOBALS
    "INTB01007.4gl"
#
# ---------------------------------------------------------------------
# main 
# ---------------------------------------------------------------------
#
MAIN

   DEFINE reci                      CHAR(1)
   DEFINE hora1                     CHAR(6)
   DEFINE f_arch                    CHAR(6)
   DEFINE hora                      CHAR(8)
   DEFINE nss                       CHAR(11)
   DEFINE v_ruta                    CHAR(70) 
   DEFINE v_arch                    CHAR(70) 
   DEFINE v_det                     CHAR(70) 
   DEFINE v_cza                     CHAR(70)
   DEFINE ejecuta                   CHAR(200)

   DEFINE farch                     DATE

   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I

   DEFER INTERRUPT       

   CALL STARTLOG(FGL_GETENV("USER")||".INTB0109.log")       
   
    LET reci = ARG_VAL(1)
    LET nss  = ARG_VAL(2)

    LET ban               = 0
    LET numero_reg        = 0
    LET consecutivo_envio = 0

    LET hoy_hoy = TODAY
    LET hora    = TIME

    INITIALIZE fentcons, n_seguro, v_det, v_cza, v_ruta, ejecuta, fecha TO NULL
    INITIALIZE farch, f_arch TO NULL

    SELECT f.*
      INTO p_tabafore.*
      FROM tab_afore_local f

    SELECT c.ruta_envio, USER
      INTO v_ruta , usuario
      FROM seg_modulo c
     WHERE c.modulo_cod = "int"

    LET hoy   = TODAY USING "mmddyy"
    LET hora1 = hora[1,2],hora[4,5],hora[7,8]

    SELECT v.nom_arch, arch_det, arch_cza 
      INTO v_arch, v_det, v_cza
      FROM tab_layout v
     WHERE v.layout_cod = 409

    IF ban <> 0 THEN
       ERROR "ERROR: Faltan comisiones"
       SLEEP 3
       EXIT PROGRAM
    END IF

    IF reci <> "S" THEN
       OPEN WINDOW INTB0109 AT 2,2 WITH FORM "INTB01091" ATTRIBUTE(BORDER)
         DISPLAY " < ESC > Procesar                                          < Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                                
         DISPLAY " INTB0109             CONSTANCIA DE TRASPASO DE REGISTRO                       " AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY hoy_hoy  USING "DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

         CONSTRUCT inter_noti ON fecha_mov_banxico
                            FROM fecha_mov_banxico

            ON KEY(ESC)
               LET ban = 0
               EXIT CONSTRUCT

            ON KEY(CONTROL-C,INTERRUPT)
                  LET ban = 1
                  EXIT CONSTRUCT

         END CONSTRUCT 

         IF ban  = 0 THEN
            LET selec_tab1 = " SELECT a.fecha_mov_banxico,\n",
                                    " a.cve_ced_cuenta,\n",
                                    " a.cve_recep_cuenta,\n",
                                    " b.n_seguro,\n",
                                    " b.n_rfc,\n",
                                    " b.n_unico,\n",
                                    " b.paterno,\n",
                                    " b.materno,\n",
                                    " b.nombres,\n",
                                    " b.sexo,\n",
                                    " b.n_folio,\n",
                                    " b.fentcons,\n",
                                    " b.tipo_solicitud,\n",
                                    " b.fecha_elaboracion,\n",
                                    " b.fena,\n",
                                    " b.nacionalidad,\n",
                                    " b.tip_prob,\n",
                                    " b.fecha_1a_afil,\n",
                                    " a.importe_bono,\n",
                                    " a.fecha_red_bono,\n",
                                    " a.folio \n",
                             " FROM  taa_viv_recepcion a, afi_mae_afiliado b, int_ctr_carta c \n",
                             " WHERE a.nss               = b.n_seguro \n",
                               " AND a.ident_operacion   = '09' \n",
                               " AND b.n_seguro          = c.nss \n",
                               " AND a.fecha_mov_banxico = c.fecha_registro \n",
                               " AND c.docto_cod         = 30221 \n",
                               " AND c.edo_genera        = 10 \n",
                               " AND b.tipo_solicitud IN(1,2,3,4,7,13,15,17,23,28,29,38,39,40,41,42,43) \n",  -- CPL-3392 --CPL-3797(38,42) --CPL-3858(39,40,41,42,43)
                               " AND b.fentcons       IS NOT NULL \n",
                               " AND a.tipo_traspaso  IN('01','02','12','21','24','71','73','75','76','77') \n", -- CPL-3392  --CPL-3797 (76)
                               " AND ",inter_noti CLIPPED," ",
                               " ORDER BY b.n_seguro " --CPL-3797

             LET farch = inter_noti[20,29]

             IF farch IS NULL OR farch = " " THEN
                LET f_arch = TODAY USING "mmddyy"
             ELSE
                LET f_arch = farch USING "mmddyy"
             END IF

             LET v_arch = v_ruta CLIPPED,"/", "30221",
                          f_arch CLIPPED,hora1 CLIPPED,".",v_arch
             LET v_cza  = v_ruta CLIPPED,"/", v_cza  CLIPPED, "_", usuario CLIPPED #CPL-1686
             LET v_det  = v_ruta CLIPPED,"/", v_det  CLIPPED, "_", usuario CLIPPED #CPL-1686

             DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." AT 19,1 ATTRIBUTE(REVERSE)

             LET leyenda_cza = "CONSTANCIA DE TRASPASO "

             START REPORT r_report TO v_det
             CALL detalle_notifica_09("21",409,2)

             START REPORT r_report TO v_cza
             CALL cabeza_notifica_09("21",leyenda_cza,409,1)

             LET ejecuta = "cat ",v_cza," ",v_det," > ",v_arch
             RUN ejecuta
             
             -- CPL-3383
             LET ejecuta = "touch mensajes_so"
             RUN ejecuta
             
             LET ejecuta = "chmod 777 mensajes_so"
             RUN ejecuta                        

             LET ejecuta = "rm ",v_cza,"  2> mensajes_so "
             RUN ejecuta

             LET ejecuta = "rm ",v_det,"  2> mensajes_so "
             RUN ejecuta
        
             LET ejecuta = "rm mensajes_so"
             RUN ejecuta  
             -- CPL-3383

             DISPLAY "" AT 19,1
             DISPLAY "ARCHIVO GENERADO EN : ",v_arch AT 19,1
             PROMPT "PROCESO FINALIZADO <ENTER> PARA CONTINUAR " FOR enter
         ELSE
            PROMPT "PROCESO CANCELADO <ENTER> PARA CONTINUAR " FOR enter
         END IF
       CLOSE WINDOW INTB0109 
    ELSE
       LET selec_tab1= " SELECT a.fecha_mov_banxico,\n",
                              " a.cve_ced_cuenta,\n",
                              " a.cve_recep_cuenta,\n",
                              " b.n_seguro,\n",
                              " b.n_rfc,\n",
                              " b.n_unico,\n",
                              " b.paterno,\n",
                              " b.materno,\n",
                              " b.nombres,\n",
                              " b.sexo,\n",
                              " b.n_folio,\n",
                              " b.fentcons,\n",
                              " b.tipo_solicitud,\n",
                              " b.fecha_elaboracion,\n",
                              " b.fena,\n",
                              " b.nacionalidad,\n",
                              " b.tip_prob,\n",
                              " b.fecha_1a_afil\n",
                              " a.folio \n",
                       " FROM  taa_viv_recepcion a, afi_mae_afiliado b \n",
                       " WHERE a.nss             = b.n_seguro \n",
                         " AND a.ident_operacion = '09' \n",
                         " AND b.n_seguro        = '", nss,"' \n",
                         " AND b.tipo_solicitud IN(2,3,7,13,15,17,23,28,29,38,42) \n",   --CPL-3392 --CPL-3797(38,42)
                         " AND b.fentcons       IS NOT NULL \n",
                         " AND a.tipo_traspaso  IN('01','02','12','21','24','71','73','75','76','77') ",  --CPL-3392  --CPL-3797 (76)
                         " ORDER BY b.n_seguro " --CPL-3797

       LET f_arch = TODAY USING "mmddyy"

       LET v_arch = v_ruta CLIPPED,"/", "30221",
                    f_arch CLIPPED,hora1 CLIPPED,".",v_arch
       LET v_cza  = v_ruta CLIPPED,"/", v_cza  CLIPPED, "_", usuario CLIPPED #CPL-1686
       LET v_det  = v_ruta CLIPPED,"/", v_det  CLIPPED, "_", usuario CLIPPED #CPL-1686

       DISPLAY "GENERANDO INFORMACION AGUARDE UN MOMENTO" AT 19,1 ATTRIBUTE(REVERSE)

       LET leyenda_cza = "CONSTANCIA DE TRASPASO "

       START REPORT r_report TO v_det
       CALL detalle_notifica_09("21",409,2)

       START REPORT r_report TO v_cza
       CALL cabeza_notifica_09("21",leyenda_cza,409,1)

       LET ejecuta = "cat ",v_cza," ",v_det," > ",v_arch
       RUN ejecuta


       -- CPL-3383
       LET ejecuta = "touch mensajes_so"
       RUN ejecuta
             
       LET ejecuta = "chmod 777 mensajes_so"
       RUN ejecuta                        

       LET ejecuta = "rm ",v_cza,"  2> mensajes_so "
       RUN ejecuta

       LET ejecuta = "rm ",v_det,"  2> mensajes_so "
       RUN ejecuta
       
       LET ejecuta = "rm mensajes_so"
       RUN ejecuta             
       -- CPL-3383

       DISPLAY "" AT 19,1
       DISPLAY "ARCHIVO GENERADO EN : ",v_arch AT 19,1
       PROMPT "PROCESO FINALIZADO < ENTER > PARA CONTINUAR" FOR enter
    END IF

END MAIN
-------------------------------------------------------------------------------