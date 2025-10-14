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
   DEFINE v_ruta, 
          v_arch,
          v_det, v_cza       CHAR(70),
          hora               CHAR(08),
          reci               CHAR(1),
          nss                CHAR(11),
          hora1              CHAR(06),
          ejecuta            CHAR(200),
          farch              DATE,
          f_arch             CHAR(06)

   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I

   DEFER INTERRUPT       
   CALL STARTLOG ("INTB0109.log")

    LET reci = ARG_VAL(1)
    LET nss  = ARG_VAL(2)

    LET ban               = 0
    LET numero_reg        = 0
    LET consecutivo_envio = 0
    LET hoy_hoy = TODAY
    LET hora    = TIME

    INITIALIZE fentcons, n_seguro, v_det, v_cza, v_ruta, ejecuta, fecha TO NULL
    INITIALIZE farch, f_arch TO NULL

    SELECT f.* INTO p_tabafore.*
        FROM tab_afore_local f

    SELECT c.ruta_envio, USER INTO v_ruta , usuario
        FROM seg_modulo c
        WHERE c.modulo_cod = "int"

    LET hoy = TODAY USING "mmddyy"
    LET hora1 = hora[1,2],hora[4,5],hora[7,8]

    SELECT v.nom_arch, arch_det, arch_cza 
        INTO  v_arch, v_det, v_cza
        FROM  tab_layout v
        WHERE v.layout_cod = 409

    CALL llena_comision_tras() RETURNING gen_compara.*,ban

    IF ban <> 0 THEN
       ERROR "ERROR: Faltan comisiones "
       SLEEP 3
       EXIT PROGRAM
    END IF

    IF reci <> "S" THEN
       OPEN WINDOW INTB0109 AT 2,2 WITH FORM "INTB01091" ATTRIBUTE(BORDER)
         DISPLAY " < ESC > Procesar                                          <",
                 " Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                                
         DISPLAY " INTB0109             CONSTANCIA DE TRASPASO DE ",
                 "REGISTRO                       "
                 AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY hoy_hoy  USING "DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

         CONSTRUCT inter_noti ON   fecha_mov_banxico
                              FROM fecha_mov_banxico

                   ON KEY(ESC)
                      LET ban = 0
                      EXIT CONSTRUCT

                   ON KEY(CONTROL-C,INTERRUPT)
                         LET ban = 1
                         EXIT CONSTRUCT
         END CONSTRUCT 

         IF ban  = 0 THEN
            LET selec_tab1= " SELECT a.fecha_mov_banxico, a.cve_ced_cuenta, ",
                            " a.cve_recep_cuenta, b.n_seguro, b.n_rfc,      ",
                            " b.n_unico, b.paterno,  b.materno,  b.nombres, ",
                            " b.sexo, b.n_folio, b.fentcons, b.tipo_solicitud,",
                            " b.fecha_elaboracion, b.fena, b.nacionalidad, ",
                            " b.tip_prob,b.fecha_1a_afil ",
                            " FROM  taa_viv_recepcion a, afi_mae_afiliado b,",
                            "       int_ctr_carta c, safre_tmp:reenvio_taa_ii d "

            LET selec_tab1= selec_tab1 CLIPPED,
                            " WHERE a.nss           = b.n_seguro",
                            " AND   a.ident_operacion = '09'    ",
                            " AND   b.n_seguro = c.nss          ",
                            " AND  a.fecha_mov_banxico=c.fecha_registro ",
                            " AND  c.docto_cod = 30221          ",
                            ---" AND  c.edo_genera = 10            ",
                            " AND  b.tipo_solicitud = 2         ",
                            " AND  b.fentcons IS NOT NULL       ",
                            " AND  a.tipo_traspaso  in(1,2)     ",
                            " AND  c.fecha_registro = d.fecha_registro",
                            " and  c.nss = d.nss",
                            " and  c.n_folio = d.n_folio"

             LET farch    = inter_noti[20,29]
             IF farch IS NULL OR farch = " " THEN
                         LET f_arch = TODAY USING "mmddyy"
             ELSE
                         LET f_arch = farch USING "mmddyy"
             END IF
{
             LET v_arch = "30221",
                          f_arch CLIPPED,hora1 CLIPPED,".",v_arch
             LET v_cza  = v_cza  CLIPPED
             LET v_det  = v_det  CLIPPED
}
             LET v_arch = v_ruta CLIPPED,"/", "30221",
                          f_arch CLIPPED,hora1 CLIPPED,".",v_arch
             LET v_cza  = v_ruta CLIPPED,"/", v_cza  CLIPPED
             LET v_det  = v_ruta CLIPPED,"/", v_det  CLIPPED

             DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." at 19,1
                     ATTRIBUTE(REVERSE)

             LET leyenda_cza = "CONSTANCIA DE TRASPASO "

             START REPORT r_report TO v_det
             CALL detalle_notifica_09("21",409,2)

             START REPORT r_report TO v_cza
             CALL cabeza_notifica_09("21",leyenda_cza,409,1)

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
       CLOSE WINDOW INTB0109 
    ELSE
            LET selec_tab1= " SELECT a.fecha_mov_banxico, a.cve_ced_cuenta, ",
                            " a.cve_recep_cuenta, b.n_seguro, b.n_rfc,      ",
                            " b.n_unico, b.paterno,  b.materno,  b.nombres, ",
                            " b.sexo, b.n_folio, b.fentcons, b.tipo_solicitud,",
                            " b.fecha_elaboracion, b.fena, b.nacionalidad, ",
                            " b.tip_prob,b.fecha_1a_afil ",
                             " FROM  taa_viv_recepcion a, afi_mae_afiliado b"

             LET selec_tab1= selec_tab1 CLIPPED,
                             " WHERE a.nss           = b.n_seguro",
                             " AND   a.ident_operacion = '09'    ",
                             " AND   b.n_seguro = ", nss,
                             " AND  b.tipo_solicitud = 2         ",
                             " AND  b.fentcons IS NOT NULL       ",
                             " AND  b.tipo_traspaso  in(1,2)     "

             LET f_arch = TODAY USING "mmddyy"

             LET v_arch = v_ruta CLIPPED,"/", "30221",
                          f_arch CLIPPED,hora1 CLIPPED,".",v_arch
             LET v_cza  = v_ruta CLIPPED,"/", v_cza  CLIPPED
             LET v_det  = v_ruta CLIPPED,"/", v_det  CLIPPED

             DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." at 19,1
                     ATTRIBUTE(REVERSE)

             LET leyenda_cza = "CONSTANCIA DE TRASPASO "

             START REPORT r_report TO v_det
             CALL detalle_notifica_09("21",409,2)

             START REPORT r_report TO v_cza
             CALL cabeza_notifica_09("21",leyenda_cza,409,1)

             LET ejecuta = "cat ",v_cza," ",v_det," > ",v_arch
             RUN ejecuta

             LET ejecuta = "rm ",v_cza," ",v_det
             RUN ejecuta

             DISPLAY "" AT 19,1
             DISPLAY "ARCHIVO GENERADO EN : ",v_arch AT 19,1
             PROMPT "PROCESO FINALIZADO...< ENTER > PARA CONTINUAR " FOR enter
    END IF
END MAIN
