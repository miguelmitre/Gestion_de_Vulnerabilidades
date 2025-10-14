#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => INTB0108 Y INTB01002                          #
#Descripcion       => NOTIFICACION DE CONSTANCIA DE REGISTRO SELEC- #
#                  => CION ADD.                                     #
#Sistema           => INT .                                         #
#Fecha             => 11 de abril del 2002   .                      #
#Por               => Laura Eugenia Cortes Guzman                   #
#*******************************************************************#
DATABASE safre_af
GLOBALS
    "INTB01002.4gl"
#
# ---------------------------------------------------------------------
# main 
# ---------------------------------------------------------------------
#
MAIN
   DEFINE v_ruta, 
          v_arch,
          v_det, v_cza       CHAR(70),
          hora               CHAR(08),
          hora1              CHAR(06),
          ejecuta            CHAR(200),
          farch              DATE,
          f_arch             CHAR(06)

   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I

##   DEFER INTERRUPT       
   CALL STARTLOG ("INTB0108.log")

    LET ban               = 0
    LET numero_reg        = 0
    LET consecutivo_envio = 0
    LET hoy_hoy = TODAY
    LET hora    = TIME

    INITIALIZE fentcons, n_seguro, v_det, v_cza, v_ruta, ejecuta, fecha TO NULL
    INITIALIZE farch, f_arch, gen_compara.* TO NULL

    CALL llena_comision_registro() RETURNING gen_compara.*,ban
    
    IF ban <> 0 THEN
       ERROR "ERROR: Faltan comisiones "
       SLEEP 3
       ERROR ""
    END IF

    SELECT f.* INTO p_tabafore.*
        FROM tab_afore_local f

    SELECT c.ruta_envio, USER 
        INTO v_ruta , usuario
        FROM seg_modulo c
        WHERE c.modulo_cod = "int"

    LET hoy = TODAY USING "mmddyy"
    LET hora1 = hora[1,2],hora[4,5],hora[7,8]

    SELECT v.nom_arch, arch_det, arch_cza 
        INTO  v_arch, v_det, v_cza
        FROM  tab_layout v
        WHERE v.layout_cod = 408

    OPEN WINDOW INTB0108 AT 2,2 WITH FORM "INTB01061" ATTRIBUTE(BORDER)
         DISPLAY " < ESC > Procesar                                          <",
		 " Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                                
         DISPLAY " INTB0108             CONSTANCIA DE SELECCION ",
		 "ADD                              "
                 AT 3,1 ATTRIBUTE(REVERSE)

         DISPLAY hoy_hoy  USING "DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

         CONSTRUCT inter_noti ON   finicta, n_seguro
                              FROM fentcons, n_seguro

                   ON KEY(ESC)
                      LET ban = 0
                      EXIT CONSTRUCT

                   ON KEY(INTERRUPT,CONTROL-C)
                         LET ban = 1
                         EXIT CONSTRUCT
         END CONSTRUCT 

         IF ban  = 0 OR ban = 1 THEN
             LET selec_tab1= " SELECT a.fecha_mov_banxico, a.cve_ced_cuenta,",
                             " b.n_seguro, b.n_rfc, b.n_unico, ",
                             " b.paterno,    b.materno,  b.nombres,b.sexo,",
                             " b.n_folio, b.fentcons, b.tipo_solicitud, ",
                             " b.fecha_elaboracion, b.fena, b.nacionalidad,",
                             " b.finitmte,b.fecha_1a_afil   ",
                             " FROM  taa_viv_recepcion a, afi_mae_afiliado b, ",
                             "       int_ctr_carta c, safre_tmp:reenvio_taa_ii d "

             LET selec_tab1= selec_tab1 CLIPPED,
                             " WHERE a.nss           = b.n_seguro   ",
                             " AND   a.ident_operacion = '09'       ",
                             " AND   a.nss             = c.nss      ",
                             " AND  a.fecha_mov_banxico=c.fecha_registro ",
                             " AND  c.docto_cod = 30203             ",
                             --" AND  c.edo_genera = 10               ",
                             " AND  b.tipo_solicitud = 1            ",
                             " AND  a.tipo_traspaso = '51'          ",
                             " AND  a.fecha_mov_banxico = b.finicta ",
                            " AND  c.fecha_registro = d.fecha_registro",
                            " and  c.nss = d.nss",
                            " and  c.n_folio = d.n_folio"

call errorlog(selec_tab1)
display selec_tab1
prompt "" for enter

             LET farch    = inter_noti[11,20]

             IF farch IS NULL OR farch = " " THEN
                         LET f_arch = TODAY USING "mmddyy"
             ELSE
                         LET f_arch = farch USING "mmddyy"
             END IF


             LET v_arch = v_ruta CLIPPED,"/", "30203",
                          f_arch CLIPPED,hora1 CLIPPED,".",v_arch

             LET v_det  = v_ruta CLIPPED,"/", v_det  CLIPPED

             LET v_cza  = v_ruta CLIPPED,"/", v_cza  CLIPPED

{
             LET v_arch = "30203",
                          f_arch CLIPPED,hora1 CLIPPED,".",v_arch
             LET v_det  = v_det  CLIPPED
             LET v_cza  = v_cza  CLIPPED
}

             DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." at 19,1
                     ATTRIBUTE(REVERSE)

             LET leyenda_cza = "CONSTANCIA DE REGISTRO "

             START REPORT r_report TO v_det
             CALL detalle_notifica_09("03",408,2)

             START REPORT r_report TO v_cza
             CALL cabeza_notifica_09("03",leyenda_cza,408,1)


             LET ejecuta = "cat ",v_cza," ",v_det," > ",v_arch
             RUN ejecuta

             LET ejecuta = "rm ",v_cza," ",v_det
             RUN ejecuta

             DISPLAY "" AT 19,1
             DISPLAY "ARCHIVO GENERADO EN : ",v_arch AT 19,1
             PROMPT "PROCESO FINALIZADO...< ENTER > PARA CONTINUAR " FOR enter
         ELSE
            PROMPT "PROCESO CANCELADO...< ENTER > PARA CONTINUAR " FOR enter
         END IF
    CLOSE WINDOW INTB0108 

END MAIN
