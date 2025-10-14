#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => INTB0118 Y INTB0100                           #
#Descripcion       => NOTIFICACION DE AVISO DE NO AFILIACION.       #
#                  =>                                               #
#Sistema           => INT .                                         #
#Fecha Elaboracion => 12 abril del 2002                             #
#Elaborado por     => LAURA EUGENIA CORTES GUZMAN                   #
#Fecha Ult.Modif.  => 12 abril del 2002                             #
#Modificado por    => LAURA EUGENIA CORTES GUZMAN                   #
#*******************************************************************#
DATABASE safre_af
GLOBALS
    "INTB0100.4gl"
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

   DEFER INTERRUPT
   CALL STARTLOG ("INTB0118.log")

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
        WHERE v.layout_cod = 418

    OPEN WINDOW INTB0118 AT 2,2 WITH FORM "INTB01061" ATTRIBUTE(BORDER)
         DISPLAY " < ESC > Procesar                                          <",
                 " Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)

         DISPLAY " INTB0118                  AVISO DE NO AFILIA",
                 "CION                              "
                 AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY hoy_hoy  USING "DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

         CONSTRUCT inter_noti ON   fentcons, n_seguro
                              FROM fentcons, n_seguro

                   ON KEY(ESC)
                      LET ban = 0
                      EXIT CONSTRUCT

                   ON KEY(INTERRUPT,CONTROL-C)
                         LET ban = 1
                         EXIT CONSTRUCT
         END CONSTRUCT


         IF ban  = 0 THEN
             LET selec_tab = " SELECT a.n_seguro,  a.n_rfc,  a.n_unico,       ",
                             " a.paterno,    a.materno,  a.nombres,           ",
                             " a.sexo, a.n_folio, a.fentcons, a.tipo_solicitud",
                             " FROM  afi_solicitud a, int_ctr_carta b "

             LET selec_tab = selec_tab CLIPPED,
                             " WHERE a.n_seguro = b.nss   ",
                             "   AND a.n_folio = b.n_folio   ",
                             "   AND a.tipo_solicitud = b.tipo_solicitud   ",
                             "   AND a.fentcons = b.fecha_registro ",
                             "   AND b.docto_cod = 30209           ",
                             "   AND b.edo_genera = 10             ",
                             "   AND  a.status_interno = 42 ",
                             "   AND a.",inter_noti CLIPPED

             LET farch    = inter_noti[11,20]
             IF farch IS NULL OR farch = " " THEN
                 LET f_arch = TODAY USING "mmddyy"
             ELSE
                 LET f_arch = farch USING "mmddyy"
             END IF
             LET v_arch = v_ruta CLIPPED,"/", "30209",
                          f_arch CLIPPED,hora1 CLIPPED,".",v_arch
             LET v_det  = v_ruta CLIPPED,"/", v_det  CLIPPED
             LET v_cza  = v_ruta CLIPPED,"/", v_cza  CLIPPED

             DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." at 19,1
                     ATTRIBUTE(REVERSE)

             LET leyenda_cza = "AVISO DE NO AFILIACION "

             START REPORT r_report TO v_det
             CALL detalle_notifica("09",418,2)

             START REPORT r_report TO v_cza
             CALL cabeza_notifica("09",leyenda_cza,418,1)

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
    CLOSE WINDOW INTB0118
END MAIN

